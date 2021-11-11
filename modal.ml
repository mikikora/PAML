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
  | Hyp of jmnt list * jmnt list * jmnt
  | ImpI of theorem * (jmnt list * jmnt list * jmnt)
  | ImpE of theorem * theorem * (jmnt list * jmnt list * jmnt)
  | HypS of jmnt list * jmnt list * jmnt
  | BoxI of theorem * (jmnt list * jmnt list * jmnt)
  | BoxE of theorem * (jmnt list * jmnt list * jmnt)
  | BoxEp of theorem * theorem * (jmnt list * jmnt list * jmnt)
  | DiaI of theorem * (jmnt list * jmnt list * jmnt)
  | DiaE of theorem * theorem * (jmnt list * jmnt list * jmnt)
  | PosI of theorem * (jmnt list * jmnt list * jmnt)

let assumption_valid th =
  match th with
  | Hyp (l, _, _)
  | ImpI (_, (l, _, _))
  | ImpE (_, _, (l, _, _))
  | HypS (l, _, _)
  | BoxI (_, (l, _, _))
  | BoxE (_, (l, _, _))
  | BoxEp (_, _, (l, _, _))
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
  | BoxE (_, (_, l, _))
  | BoxEp (_, _, (_, l, _))
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
  | BoxE (_, (_, _, x))
  | BoxEp (_, _, (_, _, x))
  | DiaI (_, (_, _, x))
  | DiaE (_, _, (_, _, x))
  | PosI (_, (_, _, x)) ->
      x

let pp_print_theorem fmtr th =
  let open Format in
  pp_open_hvbox fmtr 2;
  let rec print_assm ass =
    match ass with
    | [] -> pp_print_string fmtr "•"
    | h :: tl ->
        pp_print_jmnt fmtr h;
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
  if List.exists (function True p -> p = prop | _ -> failwith "absurd") gam
  then Hyp (del, gam, True prop)
  else failwith "can't use hyp here"

let hyps del gam prop =
  if List.exists (function Valid p -> p = prop | _ -> failwith "absurd") del
  then HypS (del, gam, True prop)
  else failwith "can't use hyps here"
