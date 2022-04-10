open Syntax
open Core
open Proof_syntax

(* Functions for creating and navigating proof *)
let proof ctx jgmt = (Empty (ctx, jgmt), Root)

let goal_desc = function
  | Empty x, path -> x
  | _ -> failwith "This is not an active goal"

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
    | Leaf _ -> failwith "cannot build goal on leaf"
  in
  if n < 1 || n > no_goals pf then failwith "There is no goal on given number"
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
    | Empty _ -> failwith "Can't build unfinished proof"
(* ------------------------------------------------------------------------- *)
(* Rules *)

(* implication introduction *)
let intro name = function
  | pf, path -> (
      match pf with
      | Empty (ctx, jgmt) -> (
          match jgmt with
          | J (x, Imp (p1, p2)) ->
              let new_ctx = add_to_ctx ~name ctx (J (x, p1)) in
              (Empty (new_ctx, J (x, p2)), Mid (path, impi (J (x, p1))))
          | _ -> failwith "Nothing to intro")
      | _ -> failwith "Not in empty goal")

(* implication, conjunction and box elimination *)
let rec apply ?names f (pf, path) =
  match pf with
  | Empty (ctx, jgmt) -> (
      if f = jgmt then (pf, path)
      else
        match (jgmt, f) with
        | J (y, prop), J (x, Imp (l, r)) ->
            if x = y then
              if r = prop then
                (Empty (ctx, f), Left (path, Empty (ctx, J (x, l)), impe))
              else
                let pf_father, path_father = apply (J (x, r)) (pf, path) in
                (Empty (ctx, f), Left (path_father, Empty (ctx, J (x, r)), impe))
            else failwith "This judgment describes other world"
        | J (y, prop), J (x, Con (p1, p2)) ->
            if x = y then
              if prop = p1 then (Empty (ctx, f), Mid (path, cone1))
              else if prop = p2 then (Empty (ctx, f), Mid (path, cone2))
              else failwith "Conjunction must have prop on either side"
            else failwith "Can't apply conjunction from different world"
        | J (y, prop), J (x, Alt (p1, p2)) ->
            if x = y then
              let new_ctx1, new_ctx2 =
                match names with
                | Some (n1, n2) ->
                    ( add_to_ctx ~name:n1 ctx (J (x, p1)),
                      add_to_ctx ~name:n2 ctx (J (x, p2)) )
                | None ->
                    (add_to_ctx ctx (J (x, p1)), add_to_ctx ctx (J (x, p2)))
              in
              ( Empty (ctx, f),
                Left3
                  (path, Empty (new_ctx1, jgmt), Empty (new_ctx2, jgmt), alte)
              )
            else failwith "Can't apply alternative from different world"
        | J (y, p), J (x, Box bp) ->
            if bp = p then
              (Empty (ctx, f), Left (path, Empty (ctx, R (x, y)), boxe x))
            else failwith "Prop doesn't match"
        | _ -> failwith "Can't use apply on this judgement")
  | _ -> failwith "Not in empty goal"

(* hyp *)

let apply_assm name (pf, path) =
  match pf with
  | Empty (ctx, jgmt) ->
      let jgmt_to_apply = List.assoc name ctx in
      let _, new_path = apply jgmt_to_apply (pf, path) in
      let ass = List.map (function name, value -> value) ctx in
      unfocus (Leaf (hyp ass jgmt_to_apply), new_path)
  | _ -> failwith "Not in empty goal"

(* False  *)
let contra world (pf, path) =
  match pf with
  | Empty (ctx, jgmt) -> (Empty (ctx, J (world, F)), Mid (path, falsee jgmt))
  | _ -> failwith "Not in empty goal"

(* conjunction introduction *)
let split (pf, path) =
  match pf with
  | Empty (ctx, jgmt) -> (
      match jgmt with
      | J (x, Con (p1, p2)) ->
          (Empty (ctx, J (x, p1)), Left (path, Empty (ctx, J (x, p2)), coni))
      | J (_, _) -> failwith "Goal is not conjunction"
      | R (_, _) -> failwith "Can't use split in this goal")
  | _ -> failwith "Not in empty goal"

(* alternative introductions *)
let left (pf, path) =
  match pf with
  | Empty (ctx, J (x, Alt (p1, p2))) ->
      (Empty (ctx, J (x, p1)), Mid (path, alti1 p2))
  | Empty (_, _) -> failwith "Goal must be alternative"
  | _ -> failwith "Not in empty goal"

let right (pf, path) =
  match pf with
  | Empty (ctx, J (x, Alt (p1, p2))) ->
      (Empty (ctx, J (x, p2)), Mid (path, alti2 p1))
  | Empty (_, _) -> failwith "Goal must be alternative"
  | _ -> failwith "Not in empty goal"

(* box introduction *)
let intro_box name world = function
  | pf, path -> (
      match pf with
      | Empty (ctx, jgmt) -> (
          match jgmt with
          | J (x, Box p) ->
              if world = x || world_in_context world ctx then
                failwith "World must not occur in this context"
              else
                ( Empty (add_to_ctx ~name ctx (R (x, world)), J (world, p)),
                  Mid (path, boxi world) )
          | _ -> failwith "Can't use intro_box on this judgement")
      | _ -> failwith "Not in empty goal")

(* diamond introduction *)
let intro_diamond world = function
  | pf, path -> (
      match pf with
      | Empty (ctx, jgmt) -> (
          match jgmt with
          | J (x, Dia p) ->
              ( Empty (ctx, J (world, p)),
                Left (path, Empty (ctx, R (x, world)), diai world) )
          | _ -> failwith "Can't use intro_diamond in this judgement")
      | _ -> failwith "not in empty goal")

(* diamond elimination *)
let apply_diamond name1 name2 world f = function
  | pf, path -> (
      match pf with
      | Empty (ctx, jgmt) -> (
          match (jgmt, f) with
          | J (z, b), J (x, Dia a) ->
              if world = z || world = x || world_in_context world ctx then
                failwith "world must not occur in this context"
              else
                let new_ctx =
                  add_to_ctx ~name:name1
                    (add_to_ctx ~name:name2 ctx (R (x, world)))
                    (J (world, a))
                in
                ( Empty (ctx, f),
                  Left (path, Empty (new_ctx, J (z, b)), diae world) )
          | _ -> failwith "can't use apply_diamond on this judgement")
      | _ -> failwith "not in empty goal")
