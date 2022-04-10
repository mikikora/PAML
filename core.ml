open Syntax

let rec remove_duplicates lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      hd :: (List.filter (function el -> el <> hd) @@ remove_duplicates tl)

let hyp rel ass jgmt =
  if List.exists (function a -> a = jgmt) ass then Hyp (rel, ass, jgmt)
  else failwith "no assumption matches goal"

let falsee new_jgmt th =
  let rel, ass, jgmt = destruct_th th in
  match (jgmt, new_jgmt) with
  | J (_, F), J (_, _) -> FalseE (th, (rel, ass, new_jgmt))
  | J (_, _), _ -> failwith "False is not in the judgement"
  | _, _ -> failwith "Can't use it on relation judgement"

let coni th1 th2 =
  let rel1, ass1, jgmt1 = destruct_th th1
  and rel2, ass2, jgmt2 = destruct_th th2 in
  if rel1 <> rel2 then failwith "Can't build theorem with different relations"
  else
    match (jgmt1, jgmt2) with
    | J (x, p1), J (y, p2) ->
        if x = y then
          ConI
            ( th1,
              th2,
              (rel1, remove_duplicates @@ ass1 @ ass2, J (x, Con (p1, p2))) )
        else failwith "worlds don't match"
    | _ -> failwith "can't use coni on this judgements"

let cone1 th =
  let rel, ass, jgmt = destruct_th th in
  match jgmt with
  | J (x, Con (a, b)) -> ConE (th, (rel, ass, J (x, a)))
  | _ -> failwith "can't use cone on this judgement"

let cone2 th =
  let rel, ass, jgmt = destruct_th th in
  match jgmt with
  | J (x, Con (a, b)) -> ConE (th, (rel, ass, J (x, b)))
  | _ -> failwith "can't use cone on this judgement"

let alti1 prop th =
  let rel, ass, jgmt = destruct_th th in
  match jgmt with
  | J (x, p) -> AltI (th, (rel, ass, J (x, Alt (p, prop))))
  | _ -> failwith "can't use alti on this judgement"

let alti2 prop th =
  let rel, ass, jgmt = destruct_th th in
  match jgmt with
  | J (x, p) -> AltI (th, (rel, ass, J (x, Alt (prop, p))))
  | _ -> failwith "can't use alti on this judgement"

let alte th1 th2 th3 =
  let rel1, ass1, jgmt1 = destruct_th th1
  and rel2, ass2, jgmt2 = destruct_th th2
  and rel3, ass3, jgmt3 = destruct_th th3 in
  if rel1 <> rel2 || rel2 <> rel3 then
    failwith "Can't build theorem with different relations"
  else
    match jgmt1 with
    | J (x, Alt (p1, p2)) ->
        if
          List.exists (function v -> v = J (x, p1)) ass2
          && List.exists (function v -> v = J (x, p2)) ass3
          && jgmt2 = jgmt3
        then
          let ass =
            List.filter (function v -> v <> J (x, p1)) (ass2 @ ass3)
          in
          AltE (th1, th2, th3, (rel1, remove_duplicates @@ ass1 @ ass, jgmt2))
        else failwith "can't use alte with this assumptions"
    | _ -> failwith "can't use alte on this judgement"

let impi left_jgmt th =
  let y, prop =
    match left_jgmt with
    | J (y, prop) -> (y, prop)
    | _ -> failwith "this judgement can't be used in implication"
  in
  let rel, ass, jgmt = destruct_th th in
  match jgmt with
  | J (x, p) ->
      if List.exists (function v -> v = left_jgmt) ass && x = y then
        ImpI
          ( th,
            ( rel,
              List.filter (function v -> v <> left_jgmt) ass,
              J (x, Imp (prop, p)) ) )
      else failwith "can't use impi with this proposition"
  | _ -> failwith "can't use impi on this judgement"

let impe th1 th2 =
  let rel1, ass1, jgmt1 = destruct_th th1
  and rel2, ass2, jgmt2 = destruct_th th2 in
  if rel1 <> rel2 then failwith "Can't build theorem with different relations"
  else
    match (jgmt1, jgmt2) with
    | J (x, Imp (p1, p2)), J (y, p3) ->
        if x = y && p1 = p3 then
          ImpE (th1, th2, (rel1, remove_duplicates @@ ass1 @ ass2, J (x, p2)))
        else failwith "can't use impe with this judgement"
    | _ -> failwith "can't use impe on this judgement"

let boxi world th =
  let rel, ass, jgmt = destruct_th th in
  match jgmt with
  | J (y, p) ->
      let matching_assumptions = assumptions_with_world y ass in
      if matching_assumptions = [ R (world, y) ] then
        BoxI (th, (rel, ass, J (world, p)))
      else failwith "can't use boxi with this assumptions"
  | _ -> failwith " can't use boxi on this judgement"

let boxe world th1 th2 =
  let rel1, ass1, jgmt1 = destruct_th th1
  and rel2, ass2, jgmt2 = destruct_th th2 in
  if rel1 <> rel2 then failwith "Can't build theorem with different relations"
  else
    match (jgmt1, jgmt2) with
    | J (x, Box p), R (y, world) ->
        if x = y then
          BoxE (th1, th2, (rel1, remove_duplicates @@ ass1 @ ass2, J (world, p)))
        else failwith "worlds don't match"
    | _ -> failwith "can't use boxe here"

let diai world th1 th2 =
  let rel1, ass1, jgmt1 = destruct_th th1
  and rel2, ass2, jgmt2 = destruct_th th2 in
  if rel1 <> rel2 then failwith "Can't build theorem with different relations"
  else
    match (jgmt1, jgmt2) with
    | J (y, p), R (world, z) ->
        if y = z then
          DiaI
            ( th1,
              th2,
              (rel1, remove_duplicates @@ ass1 @ ass2, J (world, Dia p)) )
        else failwith "worlds don't match"
    | _ -> failwith "can't use diai here"

let diae y th1 th2 =
  let rel1, ass1, jgmt1 = destruct_th th1
  and rel2, ass2, jgmt2 = destruct_th th2 in
  if rel1 <> rel2 then failwith "Can't build theorem with different relations"
  else
    match (jgmt1, jgmt2) with
    | J (x, Dia a), J (z, b) ->
        let matching_assumptions = assumptions_with_world y ass2 in
        if
          List.length matching_assumptions = 2
          && List.exists (function v -> v = R (x, y)) matching_assumptions
          && List.exists (function v -> v = J (y, a)) matching_assumptions
        then
          let ass2 =
            List.filter (function v -> v <> R (x, y) && v <> J (y, a)) ass2
          in
          DiaE (th1, th2, (rel1, remove_duplicates @@ ass1 @ ass2, J (z, b)))
        else failwith "can't use diae with this assumptions"
    | _ -> failwith "can't use diae here"
