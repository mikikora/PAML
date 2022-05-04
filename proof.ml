open Syntax
open Core
open Proof_syntax
open Relation
open Error

(* Functions for creating and navigating proof *)
let proof rel ctx jgmt = Empty (rel, ctx, jgmt)

let rec unfocus = function
  | pf, path -> (
      match path with
      | Root -> pf
      | Left (father, right, f) -> unfocus (Node2 (pf, right, f), father)
      | Right (father, left, f) -> unfocus (Node2 (left, pf, f), father)
      | Mid (father, f) -> unfocus (Node1 (pf, f), father)
      | Left3 (father, mid, right, f) ->
          unfocus (Node3 (pf, mid, right, f), father)
      | Mid3 (father, left, right, f) ->
          unfocus (Node3 (left, pf, right, f), father)
      | Right3 (father, left, mid, f) ->
          unfocus (Node3 (left, mid, pf, f), father))

let focus n pf =
  let rec build_goal acc path = function
    | Node1 (pf, f) -> build_goal acc (Mid (path, f)) pf
    | Node2 (pf1, pf2, f) ->
        let n1 = no_goals pf1 in
        if n1 >= acc then build_goal acc (Left (path, pf2, f)) pf1
        else build_goal (acc - n1) (Right (path, pf1, f)) pf2
    | Node3 (pf1, pf2, pf3, f) ->
        let n1 = no_goals pf1 in
        if n1 >= acc then build_goal acc (Left3 (path, pf2, pf3, f)) pf1
        else
          let n2 = no_goals pf3 in
          if n1 + n2 >= acc then
            build_goal (acc - n1) (Mid3 (path, pf1, pf3, f)) pf2
          else build_goal (acc - (n1 + n2)) (Right3 (path, pf1, pf2, f)) pf3
    | Empty gd -> (Empty gd, path)
    | Leaf _ -> raise (UnlocatedError "cannot build goal on leaf")
  in
  if n < 1 || n > no_goals pf then
    raise (UnlocatedError "There is no goal on given number")
  else build_goal n Root pf

let rec qed pf =
  match pf with
  | Leaf x -> x
  | Node1 (pf, f) -> f @@ qed pf
  | Node2 (pf1, pf2, f) ->
      let res1 = qed pf1 and res2 = qed pf2 in
      f res1 res2
  | Node3 (pf1, pf2, pf3, f) ->
      let res1 = qed pf1 and res2 = qed pf2 and res3 = qed pf3 in
      f res1 res2 res3
  | Empty _ -> raise (UnlocatedError "Can't build unfinished proof")

(* ------------------------------------------------------------------------- *)
(* Rules *)

(* box, diamond and implication introduction *)
let intro ?(name = None) ?(world = None) = function
  | pf, path -> (
      match pf with
      | Empty (rel, ctx, jgmt) -> (
          match jgmt with
          | J (x, Imp (p1, p2)) ->
              let new_ctx =
                (match name with
                | Some name -> add_to_ctx ~name
                | None -> (add_to_ctx : context -> judgement -> context))
                  ctx
                  (J (x, p1))
              in
              (Empty (rel, new_ctx, J (x, p2)), Mid (path, impi (J (x, p1))))
          | J (x, Box p) ->
              let w =
                match world with
                | None -> create_fresh_world_name ctx
                | Some w -> w
              in
              if w = x || world_in_context w ctx then
                raise (UnlocatedError "World must not occur in this context")
              else
                let new_ctx =
                  (match name with
                  | Some name -> add_to_ctx ~name
                  | None -> (add_to_ctx : context -> judgement -> context))
                    ctx
                    (R (x, w))
                in
                (Empty (rel, new_ctx, J (w, p)), Mid (path, boxi x))
          | J (x, Dia p) -> (
              match world with
              | None -> raise (UnlocatedError "You must specify world")
              | Some w ->
                  ( Empty (rel, ctx, J (w, p)),
                    Left (path, Empty (rel, ctx, R (x, w)), diai w) ))
          | _ -> raise (UnlocatedError "Nothing to intro"))
      | _ -> raise (UnlocatedError "Not in empty goal"))

(* implication, conjunction and box elimination *)
let rec apply ?(name1 = None) ?(name2 = None) ?(world = None) f (pf, path) =
  match pf with
  | Empty (rel, ctx, jgmt) -> (
      if f = jgmt then (pf, path) (* To prove p with p we must prove p *)
      else
        match (jgmt, f) with
        | J (y, prop), J (x, Imp (l, r)) ->
            if x = y then
              if r = prop then
                ( Empty (rel, ctx, f),
                  Left (path, Empty (rel, ctx, J (x, l)), impe) )
              else
                let pf_father, path_father = apply (J (x, r)) (pf, path) in
                ( Empty (rel, ctx, f),
                  Left (path_father, Empty (rel, ctx, J (x, r)), impe) )
            else raise (UnlocatedError "This judgment describes other world")
        | J (y, prop), J (x, Con (p1, p2)) ->
            if x = y then
              if prop = p1 then (Empty (rel, ctx, f), Mid (path, cone1))
              else if prop = p2 then (Empty (rel, ctx, f), Mid (path, cone2))
              else
                raise
                  (UnlocatedError "Conjunction must have prop on either side")
            else
              raise
                (UnlocatedError "Can't apply conjunction from different world")
        | J (y, prop), J (x, Alt (p1, p2)) ->
            if x = y then
              let new_ctx1, new_ctx2 =
                match (name1, name2) with
                | Some n1, Some n2 ->
                    ( add_to_ctx ~name:n1 ctx (J (x, p1)),
                      add_to_ctx ~name:n2 ctx (J (x, p2)) )
                | None, None ->
                    (add_to_ctx ctx (J (x, p1)), add_to_ctx ctx (J (x, p2)))
                | _ ->
                    raise
                      (UnlocatedError
                         "Two assumptions will be added. Not enugh names.")
              in
              ( Empty (rel, ctx, f),
                Left3
                  ( path,
                    Empty (rel, new_ctx1, jgmt),
                    Empty (rel, new_ctx2, jgmt),
                    alte ) )
            else
              raise
                (UnlocatedError "Can't apply alternative from different world")
        | J (y, p), J (x, Box bp) ->
            if bp = p then
              ( Empty (rel, ctx, f),
                Left (path, Empty (rel, ctx, R (x, y)), boxe x) )
            else raise (UnlocatedError "Prop doesn't match")
        | J (z, b), J (x, Dia a) ->
            let w =
              match world with
              | None -> create_fresh_world_name ctx
              | Some w -> w
            in
            if w = z || w = x || world_in_context w ctx then
              raise (UnlocatedError "World must not occur in context")
            else
              let new_ctx =
                match (name1, name2) with
                | Some name1, Some name2 ->
                    add_to_ctx ~name:name1
                      (add_to_ctx ~name:name2 ctx (R (x, w)))
                      (J (w, a))
                | None, None ->
                    add_to_ctx (add_to_ctx ctx (R (x, w))) (J (w, a))
                | _ ->
                    raise
                      (UnlocatedError
                         "Two assumptions will be added. Not enugh names.")
              in
              ( Empty (rel, ctx, f),
                Left (path, Empty (rel, new_ctx, J (z, b)), diae w) )
        | _ -> raise (UnlocatedError "Can't use apply on this judgement"))
  | _ -> raise (UnlocatedError "Not in empty goal")

(* hyp *)

let apply_assm name (pf, path) =
  match pf with
  | Empty (rel, ctx, jgmt) ->
      let jgmt_to_apply = List.assoc name ctx in
      let _, new_path = apply jgmt_to_apply (pf, path) in
      let ass = List.map (function name, value -> value) ctx in
      unfocus (Leaf (hyp rel ass jgmt_to_apply), new_path)
  | _ -> raise (UnlocatedError "Not in empty goal")

(* False  *)
let contra world (pf, path) =
  match pf with
  | Empty (rel, ctx, jgmt) ->
      (Empty (rel, ctx, J (world, F)), Mid (path, falsee jgmt))
  | _ -> raise (UnlocatedError "Not in empty goal")

(* conjunction introduction *)
let split (pf, path) =
  match pf with
  | Empty (rel, ctx, jgmt) -> (
      match jgmt with
      | J (x, Con (p1, p2)) ->
          ( Empty (rel, ctx, J (x, p1)),
            Left (path, Empty (rel, ctx, J (x, p2)), coni) )
      | J (_, _) -> raise (UnlocatedError "Goal is not conjunction")
      | R (_, _) -> raise (UnlocatedError "Can't use split in this goal"))
  | _ -> raise (UnlocatedError "Not in empty goal")

(* alternative introductions *)
let left (pf, path) =
  match pf with
  | Empty (rel, ctx, J (x, Alt (p1, p2))) ->
      (Empty (rel, ctx, J (x, p1)), Mid (path, alti1 p2))
  | Empty (_, _, _) -> raise (UnlocatedError "Goal must be alternative")
  | _ -> raise (UnlocatedError "Not in empty goal")

let right (pf, path) =
  match pf with
  | Empty (rel, ctx, J (x, Alt (p1, p2))) ->
      (Empty (rel, ctx, J (x, p2)), Mid (path, alti2 p1))
  | Empty (_, _, _) -> raise (UnlocatedError "Goal must be alternative")
  | _ -> raise (UnlocatedError "Not in empty goal")
