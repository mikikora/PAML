open Modal

type context = (string * proposition) list

type goalDesc = context * context * jmnt

type proof =
  | Empty of goalDesc
  | Node1 of proof * (theorem -> theorem)
  | Node2 of proof * proof * (theorem -> theorem -> theorem)
  | Leaf of theorem

type path =
  | Root
  | Left of path * proof * (theorem -> theorem -> theorem)
  | Right of path * proof * (theorem -> theorem -> theorem)
  | Mid of path * (theorem -> theorem)

type goal = Goal of proof * path

let rec qed pf =
  match pf with
  | Leaf x -> x
  | Node1 (pf, f) ->
      let res = qed pf in
      f res
  | Node2 (pf1, pf2, f) ->
      let res1 = qed pf1 and res2 = qed pf2 in
      f res1 res2
  | Empty _ -> failwith "Can't build unfinished proof"

let rec numGoals pf =
  match pf with
  | Empty _ -> 1
  | Leaf _ -> 0
  | Node1 (pf, _) -> numGoals pf
  | Node2 (pf1, pf2, _) -> numGoals pf1 + numGoals pf2

let rec goals pf =
  match pf with
  | Empty x -> [ x ]
  | Leaf _ -> []
  | Node1 (pf, _) -> goals pf
  | Node2 (pf1, pf2, _) -> goals pf1 @ goals pf2

let proof del gam jmnt = Empty (del, gam, jmnt)

let goal = function
  | Goal (pf, path) -> (
      match pf with Empty x -> x | _ -> failwith "this is not an active goal")

let rec unfocus = function
  | Goal (pf, path) -> (
      match path with
      | Root -> pf
      | Left (father, right, f) -> unfocus (Goal (Node2 (pf, right, f), father))
      | Right (father, left, f) -> unfocus (Goal (Node2 (left, pf, f), father))
      | Mid (father, f) -> unfocus (Goal (Node1 (pf, f), father)))

let rec next gl =
  let rec go_up (Goal (pf, path)) right =
    match pf with
    | Empty _ -> Goal (pf, path)
    | Node1 (p, f) -> go_up (Goal (p, Mid (path, f))) false
    | Node2 (p1, p2, f) ->
        if right then go_up (Goal (p2, Right (path, p1, f))) false
        else go_up (Goal (p1, Left (path, p2, f))) false
    | Leaf _ -> (
        match path with
        | Root -> failwith "Root is leaf"
        | Left (father, right, f) ->
            go_up (Goal (Node2 (pf, right, f), father)) true
        | Right (father, left, f) ->
            go_down (Goal (Node2 (pf, left, f), father))
        | Mid (father, f) -> go_down (Goal (Node1 (pf, f), father)))
  and go_down = function
    | Goal (pf, path) -> (
        match path with
        | Root -> go_up (Goal (pf, path)) false
        | Left (father, right, f) ->
            go_up (Goal (Node2 (pf, right, f), father)) true
        | Right (father, right, f) ->
            go_down (Goal (Node2 (pf, right, f), father))
        | Mid (father, f) -> go_down (Goal (Node1 (pf, f), father)))
  in
  go_up gl false

let focus n pf =
  let len = (List.length @@ goals pf) - 1 in
  let g0 = next (Goal (pf, Root)) in
  let rec aux g acc =
    if acc = n then g
    else if acc > len then failwith "too big number"
    else aux (next g) (acc + 1)
  in
  aux g0 0
