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
        | Right (father, left, f) ->
            go_down (Goal (Node2 (pf, left, f), father))
        | Mid (father, f) -> go_down (Goal (Node1 (pf, f), father)))
  in
  let (Goal (pf, path)) = gl in
  match path with
  | Root -> go_up gl false
  | Left (father, right, f) -> go_up (Goal (Node2 (pf, right, f), father)) true
  | Right (father, left, f) -> go_down (Goal (Node2 (pf, left, f), father))
  | Mid (father, f) -> go_up (Goal (Node1 (pf, f), father)) false

let focus n pf =
  let len = (List.length @@ goals pf) - 1 in
  let g0 = next (Goal (pf, Root)) in
  let rec aux g acc =
    if acc = n then g
    else if acc > len then failwith "too big number"
    else aux (next g) (acc + 1)
  in
  aux g0 0

let intro name = function
  | Goal (pf, path) -> (
      match pf with
      | Node1 (_, _) | Node2 (_, _, _) | Leaf _ -> failwith "not in empty goal"
      | Empty (del, gam, jmnt) -> (
          match jmnt with
          | True (Imp (l, r)) ->
              Goal (Empty (del, (name, l) :: gam, True r), Mid (path, impi l))
          | _ -> failwith "can't intro this judgment"))

let rec apply f gl =
  let (Goal (pf, path)) = gl in
  match pf with
  | Node1 (_, _) | Node2 (_, _, _) | Leaf _ -> failwith "not in empty goal"
  | Empty (del, gam, jmnt) -> (
      if f = jmnt then Goal (pf, path)
      else
        match f with
        | True (Imp (l, r)) -> (
            match jmnt with
            | True prop ->
                if r = prop then
                  Goal
                    ( Empty (del, gam, f),
                      Left (path, Empty (del, gam, True l), impe) )
                else
                  let (Goal (pf_father, path_father)) = apply (True r) gl in
                  Goal
                    ( Empty (del, gam, f),
                      Left (path_father, Empty (del, gam, True l), impe) )
            | _ -> failwith "can't apply implication to non true judgment")
        | _ -> failwith "can't apply this judgment here")

let apply_modal f name gl =
  let (Goal (pf, path)) = gl in
  match pf with
  | Node1 (_, _) | Node2 (_, _, _) | Leaf _ -> failwith "not in empty goal"
  | Empty (del, gam, jmnt) -> (
      match f with
      | True (Box p) ->
          Goal
            ( Empty (del, gam, True (Box p)),
              Left (path, Empty ((name, p) :: del, gam, jmnt), boxe) )
      | True (Dia p) ->
          Goal
            ( Empty (del, gam, True (Dia p)),
              Left (path, Empty (del, [ (name, p) ], jmnt), diae) )
      | _ -> failwith "can't apply this judgment here")

let apply_thm thm gl =
  let (Goal (_, new_path)) = apply (Modal.consequence thm) gl in
  unfocus (Goal (Leaf thm, new_path))

let apply_tru_assm name gl =
  let (Goal (pf, path)) = gl in
  match pf with
  | Node1 (_, _) | Node2 (_, _, _) | Leaf _ -> failwith "not in empty goal"
  | Empty (del, gam, jmnt) ->
      let prop = List.assoc name gam in
      let (Goal (_, new_path)) = apply (True prop) gl in
      let delta = List.map (function name, value -> value) del
      and gamma = List.map (function name, value -> value) gam in
      unfocus (Goal (Leaf (Modal.hyp delta gamma prop), new_path))

let apply_val_assm name gl =
  let (Goal (pf, path)) = gl in
  match pf with
  | Node1 (_, _) | Node2 (_, _, _) | Leaf _ -> failwith "not in empty goal"
  | Empty (del, gam, jmnt) ->
      let prop = List.assoc name del in
      let (Goal (_, new_path)) = apply (True prop) gl in
      let delta = List.map (function name, value -> value) del
      and gamma = List.map (function name, value -> value) gam in
      unfocus (Goal (Leaf (Modal.hyp delta gamma prop), new_path))

let from_true gl =
  let (Goal (pf, path)) = gl in
  match pf with
  | Node1 (_, _) | Node2 (_, _, _) | Leaf _ -> failwith "not in empty goal"
  | Empty (del, gam, jmnt) -> (
      match jmnt with
      | Poss p -> Goal (Empty (del, gam, True p), Mid (path, posi))
      | _ -> failwith "can't use from_true on not pos judgment")

let valid gl =
  let (Goal (pf, path)) = gl in
  match pf with
  | Node1 (_, _) | Node2 (_, _, _) | Leaf _ -> failwith "not in empty goal"
  | Empty (del, gam, jmnt) -> (
      match jmnt with
      | True (Box p) ->
          let gamma = List.map (function name, value -> value) gam in
          Goal (Empty (del, [], True p), Mid (path, boxi gamma))
      | _ -> failwith "can't use valid on not true box judgment")

let possible gl =
  let (Goal (pf, path)) = gl in
  match pf with
  | Node1 (_, _) | Node2 (_, _, _) | Leaf _ -> failwith "not in empty goal"
  | Empty (del, gam, jmnt) -> (
      match jmnt with
      | True (Dia p) -> Goal (Empty (del, gam, Poss p), Mid (path, diai))
      | _ -> failwith "can't use valid on not true box judgment")
