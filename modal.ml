type proposition =
  | P
  | Var of string
  | Imp of proposition * proposition
  | Box of proposition
  | Dia of proposition

let rec string_of_prop p =
  match p with
  | P -> "P"
  | Var x -> x
  | Imp (a, b) -> (
      match (a, b) with
      | Imp (_, _), Imp (_, _) ->
          "(" ^ string_of_prop a ^ ") -> " ^ string_of_prop b
      | Imp (_, _), _ -> "(" ^ string_of_prop a ^ ") -> " ^ string_of_prop b
      | _, Imp (_, _) -> string_of_prop a ^ " -> " ^ string_of_prop b
      | _, _ -> string_of_prop a ^ " -> " ^ string_of_prop b)
  | Box p1 -> "□" ^ string_of_prop p1
  | Dia p1 -> "◇" ^ string_of_prop p1

let pp_print_prop fmtr f = Format.pp_print_string fmtr (string_of_prop f)

type jmnt = True of proposition | Valid of proposition | Poss of proposition

let pp_print_jmnt fmtr jmnt =
  let open Format in
  match jmnt with
  | True p ->
      pp_print_prop fmtr p;
      pp_print_string fmtr " true"
  | Valid p ->
      pp_print_prop fmtr p;
      pp_print_string fmtr " valid"
  | Poss p ->
      pp_print_prop fmtr p;
      pp_print_string fmtr " poss"

type theorem =
  | Hyp of proposition list * proposition list * jmnt
  | ImpI of theorem * (proposition list * proposition list * jmnt)
  | ImpE of theorem * theorem * (proposition list * proposition list * jmnt)
  | HypS of proposition list * proposition list * jmnt
  | BoxI of theorem * (proposition list * proposition list * jmnt)
  | BoxE of theorem * theorem * (proposition list * proposition list * jmnt)
  | DiaI of theorem * (proposition list * proposition list * jmnt)
  | DiaE of theorem * theorem * (proposition list * proposition list * jmnt)
  | PosI of theorem * (proposition list * proposition list * jmnt)

let assumption_valid th =
  match th with
  | Hyp (l, _, _)
  | ImpI (_, (l, _, _))
  | ImpE (_, _, (l, _, _))
  | HypS (l, _, _)
  | BoxI (_, (l, _, _))
  | BoxE (_, _, (l, _, _))
  | DiaI (_, (l, _, _))
  | DiaE (_, _, (l, _, _))
  | PosI (_, (l, _, _)) ->
      l

let assumption_true th =
  match th with
  | Hyp (_, l, _)
  | ImpI (_, (_, l, _))
  | ImpE (_, _, (_, l, _))
  | HypS (_, l, _)
  | BoxI (_, (_, l, _))
  | BoxE (_, _, (_, l, _))
  | DiaI (_, (_, l, _))
  | DiaE (_, _, (_, l, _))
  | PosI (_, (_, l, _)) ->
      l

let consequence th =
  match th with
  | Hyp (_, _, x)
  | ImpI (_, (_, _, x))
  | ImpE (_, _, (_, _, x))
  | HypS (_, _, x)
  | BoxI (_, (_, _, x))
  | BoxE (_, _, (_, _, x))
  | DiaI (_, (_, _, x))
  | DiaE (_, _, (_, _, x))
  | PosI (_, (_, _, x)) ->
      x

let destruct_th th = (assumption_valid th, assumption_true th, consequence th)

let rec remove_duplicates lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      hd :: (List.filter (function el -> el <> hd) @@ remove_duplicates tl)

let pp_print_theorem fmtr th =
  let open Format in
  pp_open_hvbox fmtr 2;
  let rec print_assm ass =
    match ass with
    | [] -> pp_print_string fmtr "•"
    | h :: tl ->
        pp_print_prop fmtr h;
        pp_print_string fmtr ", ";
        print_assm tl
  in
  print_assm @@ assumption_valid th;
  pp_print_string fmtr "; ";
  print_assm @@ assumption_true th;
  pp_print_string fmtr "⊢";
  pp_print_jmnt fmtr @@ consequence th

let posi th =
  match consequence th with
  | True prop -> PosI (th, (assumption_valid th, assumption_true th, Poss prop))
  | _ -> failwith "can't use posi here"

let hyp del gam prop =
  if List.exists (function p -> p = prop) gam then Hyp (del, gam, True prop)
  else failwith "can't use hyp here"

let hyps del gam prop =
  if List.exists (function p -> p = prop) del then HypS (del, gam, True prop)
  else failwith "can't use hyps here"

let impi prop th =
  let del, gam, jmnt = destruct_th th in
  match jmnt with
  | True p ->
      if List.exists (function p -> p = prop) gam then
        ImpI
          ( th,
            ( del,
              List.filter (function p -> p <> prop) gam,
              True (Imp (prop, p)) ) )
      else failwith "can't use impi with this proposition here"
  | _ -> failwith "can't use impi on not true judgment"

let impe th1 th2 =
  let del1, gam1, p1 = destruct_th th1 in
  let del2, gam2, p2 = destruct_th th2 in
  match p1 with
  | True (Imp (p11, p12)) ->
      if p2 = True p11 then
        ImpE
          ( th1,
            th2,
            ( remove_duplicates @@ del1 @ del2,
              remove_duplicates @@ gam1 @ gam2,
              True p12 ) )
      else failwith "can't use impe here"
  | _ -> failwith "left judgment isn't true implication"

let boxi new_gam th =
  let del, gam, jmnt = destruct_th th in
  match jmnt with
  | True p ->
      if [] = gam then BoxI (th, (del, new_gam, True (Box p)))
      else failwith "boxi requires empty true hypotheses set"
  | _ -> failwith "boxi requires true judgment"

let boxe th1 th2 =
  let del1, gam1, p1 = destruct_th th1 and del2, gam2, p2 = destruct_th th2 in
  match p1 with
  | True (Box p11) ->
      if List.exists (function v -> v = p11) del2 then
        let del22 = List.filter (function v -> v <> p11) del2 in
        BoxE
          ( th1,
            th2,
            ( remove_duplicates @@ del1 @ del22,
              remove_duplicates @@ gam1 @ gam2,
              p2 ) )
      else failwith "no valid assumption in right theorem"
  | _ -> failwith "left judgment is not true box"

let diai th =
  match destruct_th th with
  | del, gam, Poss prop -> DiaI (th, (del, gam, True (Dia prop)))
  | _ -> failwith "can't use diai here"

let diae th1 th2 =
  let del1, gam1, p1 = destruct_th th1 and del2, gam2, p2 = destruct_th th2 in
  match (p1, p2) with
  | True (Dia p11), Poss p22 ->
      if gam2 = [ p11 ] then
        DiaE (th1, th2, (remove_duplicates @@ del1 @ del2, gam1, p2))
      else failwith "can't use diae here"
  | _, _ -> failwith "can't use diae here"
