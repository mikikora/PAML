open Syntax
open Core

type context = (string * judgement) list
let add_to_ctx ?name ctx jgmt =
  match name with
  | Some n ->
    if List.exists (function str, _ -> str = n) ctx then
      failwith (n ^ " is already in the context")
    else (n, jgmt) :: ctx
  | None -> 
    let rec generate_name n acc ->
      let name = n ^ (string_of_int acc) in
      if List.exists (function str, _ -> str = name) ctx then
        generate_name n (acc + 1)
      else name
    in
    generate_name "H" 0


type goal_desc = context * judgement

type proof =
  | Empty of goal_desc
  | Node1 of proof * (theorem -> theorem)
  | Node2 of proof * proof * (theorem -> theorem -> theorem)
  | Node3 of proof * proof * proof * (theorem -> theorem -> theorem -> theorem)
  | Leaf of theorem

type path =
  | Root
  | Left of path * proof * (theorem -> theorem -> theorem)
  | Right of path * proof * (theorem -> theorem -> theorem)
  | Mid of path * (theorem -> theorem)
  (*
     In 3 node proofs always goes: left, mid, right (without the one from the constructor)
     Sof of example in Left3 it goes: mid, right and in Right3: left, mid
  *)
  | Left3 of path * proof * proof * (theorem -> theorem -> theorem -> theorem)
  | Mid3 of path * proof * proof * (theorem -> theorem -> theorem -> theorem)
  | Right3 of path * proof * proof * (theorem -> theorem -> theorem -> theorem)

type goal = proof * path

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

let rec no_goals pf =
  match pf with
  | Empty _ -> 1
  | Leaf _ -> 0
  | Node1 (pf, _) -> no_goals pf
  | Node2 (pf1, pf2, _) -> no_goals pf1 + no_goals pf2
  | Node3 (pf1, pf2, pf3, _) -> no_goals pf1 + no_goals pf2 + no_goals pf3

let rec goals pf =
  match pf with
  | Empty x -> [ x ]
  | Leaf _ -> []
  | Node1 (pf, _) -> goals pf
  | Node2 (pf1, pf2, _) -> goals pf1 @ goals pf2
  | Node3 (pf1, pf2, pf3, _) -> goals pf1 @ goals pf2 @ goals pf3

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

let focus n gl =
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
  let pf, path = gl in
  if n < 1 || n > no_goals pf then failwith "There is no goal on given number"
  else build_goal n path pf

(* implication introduction *)
let intro name = function
  | pf, path -> (
      match pf with
      | Empty (ctx, jgmt) -> (
          match jgmt with
          | J (x, Imp (p1, p2)) ->
            let new_ctx = add_to_ctx ~name:name ctx J(x, p1) in
            (Empty(new_ctx, J(x, p2)), Mid(path, impi (J(x,p1))))
          | _ -> failwith "Nothing to intro")
      | _ -> failwith "Not in empty goal")

(* implication and conjunction elimination *)
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
        | J(y, prop), J(x, Alt(p1, p2)) ->
          if x = y then (
            match name with
            | Some(n1, n2) -> 
              let new_ctx1 = add_to_ctx ~name:n1 ctx (J(x, p1))
              and new_ctx2 = add_to_ctx ~name:n2 ctx (J(x, p2)) in
              (Empty(ctx, f),
              Left3(path, Empty(new_ctx1, jgmt), Empty(new_ctx2, jgmt), alte))
          )
          else failwith "Can't apply alternative from different world"
            )
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
  | Empty(ctx, J(x, Alt(p1, p2))) ->
    (Empty(ctx, J(x, p1)), Mid(path, alti1))
  | Empty(_, _) -> failwith "Goal must be alternative"
  | _ -> failwith "Not in empty goal"

let right (pf, path) =
  match pf with
  | Empty(ctx, J(x, Alt(p1, p2))) ->
    (Empty(ctx, J(x, p2)), Mid(path, alti1))
  | Empty(_, _) -> failwith "Goal must be alternative"
  | _ -> failwith "Not in empty goal"
