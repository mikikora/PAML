open Format

type prop =
  | F
  | Var of string
  | Con of prop * prop
  | Alt of prop * prop
  | Imp of prop * prop
  | Box of prop
  | Dia of prop

type world = string
type relation = world * world
type judgement = J of world * prop | R of relation
type assumptions = judgement list

type theorem =
  | FalseE of theorem * (assumptions * judgement)
  | Hyp of assumptions * judgement
  | ConI of theorem * theorem * (assumptions * judgement)
  | ConE of theorem * (assumptions * judgement)
  | AltI of theorem * (assumptions * judgement)
  | AltE of theorem * theorem * theorem * (assumptions * judgement)
  | ImpI of theorem * (assumptions * judgement)
  | ImpE of theorem * theorem * (assumptions * judgement)
  | BoxI of theorem * (assumptions * judgement)
  | BoxE of theorem * theorem * (assumptions * judgement)
  | DiaI of theorem * theorem * (assumptions * judgement)
  | DiaE of theorem * theorem * (assumptions * judgement)

(* supporting functions *)

let assumptions = function
  | FalseE (_, (l, _))
  | Hyp (l, _)
  | ConI (_, _, (l, _))
  | ConE (_, (l, _))
  | AltI (_, (l, _))
  | AltE (_, _, _, (l, _))
  | ImpI (_, (l, _))
  | ImpE (_, _, (l, _))
  | BoxI (_, (l, _))
  | BoxE (_, _, (l, _))
  | DiaI (_, _, (l, _))
  | DiaE (_, _, (l, _)) ->
      l

let consequence = function
  | FalseE (_, (_, x))
  | Hyp (_, x)
  | ConI (_, _, (_, x))
  | ConE (_, (_, x))
  | AltI (_, (_, x))
  | AltE (_, _, _, (_, x))
  | ImpI (_, (_, x))
  | ImpE (_, _, (_, x))
  | BoxI (_, (_, x))
  | BoxE (_, _, (_, x))
  | DiaI (_, _, (_, x))
  | DiaE (_, _, (_, x)) ->
      x

let destruct_th th = (assumptions th, consequence th)

(* printers *)

let rec pp_print_theorem fmtr th =
  let ass, jgmt = destruct_th th in
  pp_open_hvbox fmtr 0;
  pp_print_assumptions fmtr ass;
  pp_print_space fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_judgement fmtr jgmt;
  pp_close_box fmtr ()

and pp_print_assumptions fmtr ass =
  if ass = [] then pp_print_string fmtr "•" else pp_open_hvbox fmtr 0;
  List.iter
    (function
      | a ->
          pp_print_judgement fmtr a;
          pp_print_string fmtr ";";
          pp_print_space fmtr ())
    ass;
  pp_close_box fmtr ()

and pp_print_judgement fmtr = function
  | R (x, y) ->
      pp_print_string fmtr x;
      pp_print_string fmtr "R";
      pp_print_string fmtr y
  | J (world, p) ->
      pp_open_hvbox fmtr 0;
      pp_print_string fmtr world;
      pp_print_string fmtr ":";
      pp_print_space fmtr ();
      pp_print_imp_prop fmtr p;
      pp_close_box fmtr ()

and pp_print_imp_prop fmtr = function
  | Imp (p1, p2) ->
      pp_print_alt_prop fmtr p1;
      pp_print_space fmtr ();
      pp_print_string fmtr "⊃";
      pp_print_space fmtr ();
      pp_print_imp_prop fmtr p2
  | _ as p -> pp_print_alt_prop fmtr p

and pp_print_alt_prop fmtr = function
  | Alt (p1, p2) ->
      pp_print_con_prop fmtr p1;
      pp_print_space fmtr ();
      pp_print_string fmtr "∨";
      pp_print_space fmtr ();
      pp_print_con_prop fmtr p2
  | _ as p -> pp_print_con_prop fmtr p

and pp_print_con_prop fmtr = function
  | Con (p1, p2) ->
      pp_print_atom_prop fmtr p1;
      pp_print_space fmtr ();
      pp_print_string fmtr "∧";
      pp_print_space fmtr ();
      pp_print_atom_prop fmtr p2
  | _ as p -> pp_print_atom_prop fmtr p

and pp_print_atom_prop fmtr = function
  | F -> pp_print_string fmtr "⊥"
  | Var x -> pp_print_string fmtr x
  | Box p ->
      pp_print_string fmtr "◻";
      pp_print_atom_prop fmtr p
  | Dia p ->
      pp_print_string fmtr "◇";
      pp_print_atom_prop fmtr p
  | _ as p ->
      pp_open_hvbox fmtr 1;
      pp_print_string fmtr "(";
      pp_print_cut fmtr ();
      pp_print_imp_prop fmtr p;
      pp_print_cut fmtr ();
      pp_print_string fmtr ")";
      pp_close_box fmtr ()

let print_theorem th = pp_print_theorem std_formatter th
let print_judgement jgmt = pp_print_judgement std_formatter jgmt
