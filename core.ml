open Syntax

let rec remove_duplicates lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      hd :: (List.filter (function el -> el <> hd) @@ remove_duplicates tl)

let hyp rel ass jgmt =
  if List.mem jgmt ass then Assumption (Hyp, (rel, ass, jgmt))
  else failwith "no assumption matches goal"

let weakening jgmt_to_add th =
  let rel, ass, jgmt = destruct_th th in
  Single (Weak, th, (rel, remove_duplicates @@ (jgmt_to_add :: ass), jgmt))

let falsee new_jgmt th =
  let rel, ass, jgmt = destruct_th th in
  match (jgmt, new_jgmt) with
  | J (_, F), J (_, _) -> Single (FalseE, th, (rel, ass, new_jgmt))
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
          Double
            ( ConI,
              th1,
              th2,
              (rel1, remove_duplicates @@ ass1 @ ass2, J (x, Con (p1, p2))) )
        else failwith "worlds don't match"
    | _ -> failwith "can't use coni on this judgements"

let cone1 th =
  let rel, ass, jgmt = destruct_th th in
  match jgmt with
  | J (x, Con (a, b)) -> Single (ConE1, th, (rel, ass, J (x, a)))
  | _ -> failwith "can't use cone on this judgement"

let cone2 th =
  let rel, ass, jgmt = destruct_th th in
  match jgmt with
  | J (x, Con (a, b)) -> Single (ConE2, th, (rel, ass, J (x, b)))
  | _ -> failwith "can't use cone on this judgement"

let alti1 prop th =
  let rel, ass, jgmt = destruct_th th in
  match jgmt with
  | J (x, p) -> Single (AltI1, th, (rel, ass, J (x, Alt (p, prop))))
  | _ -> failwith "can't use alti on this judgement"

let alti2 prop th =
  let rel, ass, jgmt = destruct_th th in
  match jgmt with
  | J (x, p) -> Single (AltI2, th, (rel, ass, J (x, Alt (prop, p))))
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
        if jgmt1 <> jgmt2 then
          failwith "Judgements in theorem 2 and 3 must be the same"
        else
          let new_th2 =
            if List.mem (J (x, p1)) ass2 then th2 else weakening (J (x, p1)) th2
          in
          let new_th3 =
            if List.mem (J (x, p2)) ass3 then th3 else weakening (J (x, p2)) th3
          in
          let ass =
            List.filter
              (function v -> v <> J (x, p1) && v <> J (x, p2))
              (assumptions new_th2 @ assumptions new_th3)
          in
          Triple
            ( AltE,
              th1,
              new_th2,
              new_th3,
              (rel1, remove_duplicates @@ ass1 @ ass, jgmt2) )
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
      let new_th =
        if List.mem left_jgmt ass then th else weakening left_jgmt th
      in
      if x = y then
        Single
          ( ImpI,
            new_th,
            ( rel,
              List.filter (function v -> v <> left_jgmt) (assumptions new_th),
              J (x, Imp (prop, p)) ) )
      else failwith "can't use impi with different worlds in judgements"
  | _ -> failwith "can't use impi on this judgement"

let impe th1 th2 =
  let rel1, ass1, jgmt1 = destruct_th th1
  and rel2, ass2, jgmt2 = destruct_th th2 in
  if rel1 <> rel2 then failwith "Can't build theorem with different relations"
  else
    match (jgmt1, jgmt2) with
    | J (x, Imp (p1, p2)), J (y, p3) ->
        if x = y && p1 = p3 then
          Double
            (ImpE, th1, th2, (rel1, remove_duplicates @@ ass1 @ ass2, J (x, p2)))
        else failwith "can't use impe with this judgement"
    | _ -> failwith "can't use impe on this judgement"

let boxi world th =
  let rel, ass, jgmt = destruct_th th in
  match jgmt with
  | J (y, p) ->
      let new_th =
        if List.mem (R (world, y)) ass then th else weakening (R (world, y)) th
      in
      let new_ass = assumptions new_th in
      let matching_assumptions = assumptions_with_world y new_ass in
      if matching_assumptions = [ R (world, y) ] then
        let result_ass =
          List.filter (function elem -> elem <> R (world, y)) new_ass
        in
        Single (BoxI, new_th, (rel, result_ass, J (world, Box p)))
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
          Double
            ( BoxE,
              th1,
              th2,
              (rel1, remove_duplicates @@ ass1 @ ass2, J (world, p)) )
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
          Double
            ( DiaI,
              th1,
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
        let new_th2 =
          if List.mem (J (y, a)) ass2 then th2 else weakening (J (y, a)) th2
        in
        let new_th2 =
          if List.mem (R (x, y)) ass2 then new_th2 else weakening (R (x, y)) th2
        in
        let new_ass2 = assumptions new_th2 in
        let matching_assumptions = assumptions_with_world y new_ass2 in
        if List.length matching_assumptions = 2 then
          let new_ass2 =
            List.filter
              (function v -> v <> R (x, y) && v <> J (y, a))
              new_ass2
          in
          Double
            ( DiaE y,
              th1,
              new_th2,
              (rel1, remove_duplicates @@ ass1 @ new_ass2, J (z, b)) )
        else failwith "can't use diae with this assumptions"
    | _ -> failwith "can't use diae here"

let seriality x y th =
  let rel, ass, jgmt = destruct_th th in
  if Relation.has_property Relation.Seriality rel then
    match jgmt with
    | J (z, prop) ->
        let new_th =
          if List.mem (R (x, y)) ass then th else weakening (R (x, y)) th
        in
        let matching_assumptions = assumptions_with_world y ass in
        if y = x || y = z || matching_assumptions <> [ R (x, y) ] then
          failwith "can't use seriality with this assumptions"
        else
          let new_ass =
            List.filter (function v -> v <> R (x, y)) (assumptions new_th)
          in
          Single (D (x, y), new_th, (rel, new_ass, jgmt))
    | _ -> failwith "can't use seriality here"
  else failwith "seriality can only be used with seriable relation"

let reflexivity x th =
  let rel, ass, jgmt = destruct_th th in
  if Relation.has_property Relation.Reflexivity rel then
    match jgmt with
    | J (y, prop) ->
        let new_th =
          if List.mem (R (x, x)) ass then th else weakening (R (x, x)) th
        in
        let new_ass = assumptions new_th in
        if List.mem (R (x, x)) (assumptions_with_world x new_ass) then
          let new_ass = List.filter (function v -> v <> R (x, x)) new_ass in
          Single (T x, new_th, (rel, new_ass, jgmt))
        else
          failwith
            "There is no reflexive assumption with this world in the scope"
    | _ -> failwith "can't use reflexivity here"
  else failwith "reflexivity can only be used with reflexivitive relation"

let symmetry th1 th2 =
  let rel1, ass1, jgmt1 = destruct_th th1
  and rel2, ass2, jgmt2 = destruct_th th2 in
  if rel1 = rel2 && Relation.has_property Relation.Symmetry rel1 then
    match jgmt1 with
    | R (x, y) ->
        let new_th2 =
          if List.mem (R (y, x)) ass2 then th2 else weakening (R (y, x)) th2
        in
        let ass2 = assumptions new_th2 in
        if List.mem (R (y, x)) ass2 then
          let new_ass2 = List.filter (function v -> v <> R (y, x)) ass2 in
          let new_ass = remove_duplicates @@ ass1 @ new_ass2 in
          Double (B, th1, new_th2, (rel1, new_ass, jgmt2))
        else
          failwith
            "There is no symmetry assumption with this worlds in the scope"
    | _ -> failwith "can't use symmetry here"
  else
    failwith
      "Can't build theorem with different relations or with non symmetrical \
       relation"

let transitivity th1 th2 th3 =
  let rel1, ass1, jgmt1 = destruct_th th1
  and rel2, ass2, jgmt2 = destruct_th th2
  and rel3, ass3, jgmt3 = destruct_th th3 in
  if
    rel1 = rel2 && rel2 = rel3
    && Relation.has_property Relation.Transitivity rel1
  then
    match (jgmt1, jgmt2) with
    | R (x, y1), R (y2, z) ->
        let new_th3 =
          if List.mem (R (x, z)) ass3 then th3 else weakening (R (x, z)) th3
        in
        let ass3 = assumptions new_th3 in
        if y1 = y2 && List.mem (R (x, z)) ass3 then
          let new_ass3 = List.filter (function v -> v <> R (x, z)) ass3 in
          let new_ass = remove_duplicates @@ ass1 @ ass2 @ new_ass3 in
          Triple (Four, th1, th2, new_th3, (rel1, new_ass, jgmt3))
        else failwith "Premises can't build this rule"
    | _, _ -> failwith "can't use transitivity here"
  else
    failwith
      "Can't build theorem with different relations or with non transitive \
       relation"

let euclideanness th1 th2 th3 =
  let rel1, ass1, jgmt1 = destruct_th th1
  and rel2, ass2, jgmt2 = destruct_th th2
  and rel3, ass3, jgmt3 = destruct_th th3 in
  if
    rel1 = rel2 && rel2 = rel3
    && Relation.has_property Relation.Euclideanness rel1
  then
    match (jgmt1, jgmt2) with
    | R (x1, y), R (x2, z) ->
        let new_th3 =
          if List.mem (R (y, z)) ass3 then th3 else weakening (R (y, z)) th3
        in
        let ass3 = assumptions new_th3 in
        if x1 = x2 && List.mem (R (y, z)) ass3 then
          let new_ass3 = List.filter (function v -> v <> R (y, z)) ass3 in
          let new_ass = remove_duplicates @@ ass1 @ ass2 @ new_ass3 in
          Triple (Five, th1, th2, new_th3, (rel1, new_ass, jgmt3))
        else failwith "Premises can't build this rule"
    | _, _ -> failwith "can't use euclideanness here"
  else
    failwith
      "Can't build theorem with different relations or with non euclidean \
       relation"

let directedness w th1 th2 th3 =
  let rel1, ass1, jgmt1 = destruct_th th1
  and rel2, ass2, jgmt2 = destruct_th th2
  and rel3, ass3, jgmt3 = destruct_th th3 in
  if
    rel1 = rel2 && rel2 = rel3
    && Relation.has_property Relation.Directedness rel1
  then
    match (jgmt1, jgmt2, jgmt3) with
    | R (x1, y), R (x2, z), J (v, _) ->
        let new_th3 =
          if List.mem (R (y, w)) ass3 then th3 else weakening (R (y, w)) th3
        in
        let new_th3 =
          if List.mem (R (z, w)) ass3 then new_th3
          else weakening (R (z, w)) new_th3
        in
        let ass3 = assumptions new_th3 in
        let matching_assumptions = assumptions_with_world w ass3 in
        if
          x1 = x2 && w <> x1 && w <> y && w <> z && w <> v
          && List.length matching_assumptions = 2
        then
          let new_ass3 =
            List.filter (function v -> v <> R (y, w) && v <> R (z, w)) ass3
          in
          let new_ass = remove_duplicates @@ ass1 @ ass2 @ new_ass3 in
          Triple (Two w, th1, th2, new_th3, (rel1, new_ass, jgmt3))
        else failwith "Premises can't build this rule"
    | _, _, _ -> failwith "can't use directedness here"
  else
    failwith
      "Can't build theorem with different relations or with non directed \
       relation"

(* Check if given theorem is valid *)
let rec validate_theorem th =
  let absurd_theorem = Assumption (Hyp, ("", [], J ("", F))) in
  let calculated_theorem =
    match th with
    | Assumption (rule, (rel, ass, jgmt)) -> hyp rel ass jgmt
    | Single (rule, th1, (_, ass, jgmt)) ->
        if validate_theorem th1 then
          match rule with
          | Weak ->
              let prev_ass = assumptions th1 in
              if
                List.filter (fun elem -> not @@ List.mem elem ass) prev_ass = []
                && List.length
                     (List.filter
                        (fun elem -> not @@ List.mem elem prev_ass)
                        ass)
                   = 1
              then th
              else absurd_theorem
          | FalseE -> falsee jgmt th1
          | ConE1 -> cone1 th1
          | ConE2 -> cone2 th1
          | AltI1 -> (
              match jgmt with
              | J (_, Alt (_, prop)) -> alti1 prop th1
              | _ -> absurd_theorem)
          | AltI2 -> (
              match jgmt with
              | J (_, Alt (prop, _)) -> alti1 prop th1
              | _ -> absurd_theorem)
          | ImpI -> (
              match jgmt with
              | J (x, Imp (prop, _)) -> impi (J (x, prop)) th1
              | _ -> absurd_theorem)
          | BoxI -> (
              match jgmt with J (x, _) -> boxi x th1 | _ -> absurd_theorem)
          | D (x, y) -> seriality x y th1
          | T x -> reflexivity x th1
          | _ -> absurd_theorem
        else absurd_theorem
    | Double (rule, th1, th2, (_, _, jgmt)) ->
        if validate_theorem th1 && validate_theorem th2 then
          match rule with
          | ConI -> coni th1 th2
          | ImpE -> impe th1 th2
          | BoxE -> (
              match jgmt with J (x, _) -> boxe x th1 th2 | _ -> absurd_theorem)
          | DiaI -> (
              match jgmt with J (x, _) -> diai x th1 th2 | _ -> absurd_theorem)
          | DiaE x -> diae x th1 th2
          | B -> symmetry th1 th2
          | _ -> absurd_theorem
        else absurd_theorem
    | Triple (rule, th1, th2, th3, (_, _, jgmt)) ->
        if validate_theorem th1 && validate_theorem th2 && validate_theorem th3
        then
          match rule with
          | AltE -> alte th1 th2 th3
          | Four -> transitivity th1 th2 th3
          | Five -> euclideanness th1 th2 th3
          | Two x -> directedness x th1 th2 th3
          | _ -> absurd_theorem
        else absurd_theorem
  in
  calculated_theorem <> absurd_theorem && th = calculated_theorem
