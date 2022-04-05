open Syntax

let rec remove_duplicates lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      hd :: (List.filter (function el -> el <> hd) @@ remove_duplicates tl)

let hyp ass jgmt =
  if List.exists (function a -> a = jgmt) ass then Hyp (ass, jgmt)
  else failwith "no assumption matches goal"

let falsee new_jgmt th =
  let ass, jgmt = destruct_th th in
  match (jgmt, new_jgmt) with
  | J (_, F), J (_, _) -> FalseE (th, (ass, new_jgmt))
  | J (_, _), _ -> failwith "False is not in the judgement"
  | _, _ -> failwith "Can't use it on relation judgement"

let coni th1 th2 =
  let ass1, jgmt1 = destruct_th th1 and ass2, jgmt2 = destruct_th th2 in
  match (jgmt1, jgmt2) with
  | J (x, p1), J (y, p2) ->
      if x = y then
        ConI (th1, th2, (remove_duplicates @@ ass1 @ ass2, J (x, Con (p1, p2))))
      else failwith "worlds don't match"
  | _ -> failwith "can't use coni on this judgements"

let cone1 thm =
  let ass, jgmt = destruct_th thm in
  match jgmt with
  | J (x, Con (a, b)) -> ConE (thm, (ass, J (x, a)))
  | _ -> failwith "can't use cone on this judgement"

let cone2 thm =
  let ass, jgmt = destruct_th thm in
  match jgmt with
  | J (x, Con (a, b)) -> ConE (thm, (ass, J (x, b)))
  | _ -> failwith "can't use cone on this judgement"

let alti1 thm prop =
  let ass, jgmt = destruct_th thm in
  match jgmt with
  | J (x, p) -> AltI (thm, (ass, J (x, Alt (p, prop))))
  | _ -> failwith "can't use alti on this judgement"

let alti2 thm prop =
  let ass, jgmt = destruct_th thm in
  match jgmt with
  | J (x, p) -> AltI (thm, (ass, J (x, Alt (prop, p))))
  | _ -> failwith "can't use alti on this judgement"

let alte thm1 thm2 thm3 =
  let ass1, jgmt1 = destruct_th thm1
  and ass2, jgmt2 = destruct_th thm2
  and ass3, jgmt3 = destruct_th thm3 in
  match jgmt1 with
  | J (x, Alt (p1, p2)) ->
      if
        List.exists (function v -> v = J (x, p1)) ass2
        && List.exists (function v -> v = J (x, p2)) ass3
        && jgmt2 = jgmt3
      then
        let ass = List.filter (function v -> v <> J (x, p1)) (ass2 @ ass3) in
        AltE (thm1, thm2, thm3, (remove_duplicates @@ ass1 @ ass, jgmt2))
      else failwith "can't use alte with this assumptions"
  | _ -> failwith "can't use alte on this judgement"

let impi left_jgmt th =
  let y, prop =
    match left_jgmt with
    | J (y, prop) -> (y, prop)
    | _ -> failwith "this judgement can't be used in implication"
  in
  let ass, jgmt = destruct_th th in
  match jgmt with
  | J (x, p) ->
      if List.exists (function v -> v = left_jgmt) ass && x = y then
        ImpI
          ( th,
            ( List.filter (function v -> v <> left_jgmt) ass,
              J (x, Imp (prop, p)) ) )
      else failwith "can't use impi with this proposition"
  | _ -> failwith "can't use impi on this judgement"

let impe thm1 thm2 =
  let ass1, jgmt1 = destruct_th thm1 and ass2, jgmt2 = destruct_th thm2 in
  match (jgmt1, jgmt2) with
  | J (x, Imp (p1, p2)), J (y, p3) ->
      if x = y && p1 = p3 then
        ImpE (thm1, thm2, (remove_duplicates @@ ass1 @ ass2, J (x, p2)))
      else failwith "can't use impe with this judgement"
  | _ -> failwith "can't use impe on this judgement"

let boxi thm world =
  let world_in_assumption world assumption =
    match assumption with
    | R (w1, w2) -> world = w1 || world = w2
    | J (w, _) -> w = world
  in
  let ass, jgmt = destruct_th thm in
  match jgmt with
  | J (y, p) ->
      let matching_assumptions = List.filter (world_in_assumption y) ass in
      if matching_assumptions = [ R (world, y) ] then
        BoxI (thm, (ass, J (world, p)))
      else failwith "can't use boxi with this assumptions"
  | _ -> failwith " can't use boxi on this judgement"

let boxe thm1 thm2 world =
  let ass1, jgmt1 = destruct_th thm1 and ass2, jgmt2 = destruct_th thm2 in
  match (jgmt1, jgmt2) with
  | J (x, Box p), R (y, world) ->
      if x = y then
        BoxE (thm1, thm2, (remove_duplicates @@ ass1 @ ass2, J (world, p)))
      else failwith "worlds don't match"
  | _ -> failwith "can't use boxe here"

let diai thm1 thm2 world =
  let ass1, jgmt1 = destruct_th thm1 and ass2, jgmt2 = destruct_th thm2 in
  match (jgmt1, jgmt2) with
  | J (y, p), R (world, z) ->
      if y = z then
        DiaI (thm1, thm2, (remove_duplicates @@ ass1 @ ass2, J (world, Dia p)))
      else failwith "worlds don't match"
  | _ -> failwith "can't use diai here"

let diae thm1 thm2 y =
  let world_in_assumption world assumption =
    match assumption with
    | R (w1, w2) -> world = w1 || world = w2
    | J (w, _) -> w = world
  in
  let ass1, jgmt1 = destruct_th thm1 and ass2, jgmt2 = destruct_th thm2 in
  match (jgmt1, jgmt2) with
  | J (x, Dia a), J (z, b) ->
      let matching_assumptions = List.filter (world_in_assumption y) ass2 in
      if
        List.length matching_assumptions = 2
        && List.exists (function v -> v = R (x, y)) matching_assumptions
        && List.exists (function v -> v = J (y, a)) matching_assumptions
      then
        let ass2 =
          List.filter (function v -> v <> R (x, y) && v <> J (y, a)) ass2
        in
        DiaE (thm1, thm2, (remove_duplicates @@ ass1 @ ass2, J (z, b)))
      else failwith "can't use diae with this assumptions"
  | _ -> failwith "can't use diae here"
