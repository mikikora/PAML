open Format
open Relation

type prop =
  | F
  | Var of string
  | Con of prop * prop
  | Alt of prop * prop
  | Imp of prop * prop
  | Box of prop
  | Dia of prop

type world = string
type judgement = J of world * prop | R of world * world
type assumptions = judgement list
type theorem_context = string * assumptions * judgement

type theorem =
  | FalseE of theorem * theorem_context
  | Hyp of theorem_context
  | ConI of theorem * theorem * theorem_context
  | ConE of theorem * theorem_context
  | AltI of theorem * theorem_context
  | AltE of theorem * theorem * theorem * theorem_context
  | ImpI of theorem * theorem_context
  | ImpE of theorem * theorem * theorem_context
  | BoxI of theorem * theorem_context
  | BoxE of theorem * theorem * theorem_context
  | DiaI of theorem * theorem * theorem_context
  | DiaE of theorem * theorem * theorem_context
  | D of theorem * theorem_context
  | T of theorem * theorem_context
  | B of theorem * theorem * theorem_context
  | Four of theorem * theorem * theorem * theorem_context
  | Five of theorem * theorem * theorem * theorem_context
  | Two of theorem * theorem * theorem * theorem_context

(* supporting functions *)

let relation = function
  | FalseE (_, (r, _, _))
  | Hyp (r, _, _)
  | ConI (_, _, (r, _, _))
  | ConE (_, (r, _, _))
  | AltI (_, (r, _, _))
  | AltE (_, _, _, (r, _, _))
  | ImpI (_, (r, _, _))
  | ImpE (_, _, (r, _, _))
  | BoxI (_, (r, _, _))
  | BoxE (_, _, (r, _, _))
  | DiaI (_, _, (r, _, _))
  | DiaE (_, _, (r, _, _))
  | D (_, (r, _, _))
  | T (_, (r, _, _))
  | B (_, _, (r, _, _))
  | Four (_, _, _, (r, _, _))
  | Five (_, _, _, (r, _, _))
  | Two (_, _, _, (r, _, _)) ->
      r

let assumptions = function
  | FalseE (_, (_, l, _))
  | Hyp (_, l, _)
  | ConI (_, _, (_, l, _))
  | ConE (_, (_, l, _))
  | AltI (_, (_, l, _))
  | AltE (_, _, _, (_, l, _))
  | ImpI (_, (_, l, _))
  | ImpE (_, _, (_, l, _))
  | BoxI (_, (_, l, _))
  | BoxE (_, _, (_, l, _))
  | DiaI (_, _, (_, l, _))
  | DiaE (_, _, (_, l, _))
  | D (_, (_, l, _))
  | T (_, (_, l, _))
  | B (_, _, (_, l, _))
  | Four (_, _, _, (_, l, _))
  | Five (_, _, _, (_, l, _))
  | Two (_, _, _, (_, l, _)) ->
      l

let consequence = function
  | FalseE (_, (_, _, x))
  | Hyp (_, _, x)
  | ConI (_, _, (_, _, x))
  | ConE (_, (_, _, x))
  | AltI (_, (_, _, x))
  | AltE (_, _, _, (_, _, x))
  | ImpI (_, (_, _, x))
  | ImpE (_, _, (_, _, x))
  | BoxI (_, (_, _, x))
  | BoxE (_, _, (_, _, x))
  | DiaI (_, _, (_, _, x))
  | DiaE (_, _, (_, _, x))
  | D (_, (_, _, x))
  | T (_, (_, _, x))
  | B (_, _, (_, _, x))
  | Four (_, _, _, (_, _, x))
  | Five (_, _, _, (_, _, x))
  | Two (_, _, _, (_, _, x)) ->
      x

let destruct_th th = (relation th, assumptions th, consequence th)

(* For modal rules function assumptions_with_world will be helpful *)
let assumptions_with_world world assumptions =
  List.filter
    (function
      | ass -> (
          match ass with
          | R (w1, w2) -> world = w1 || world = w2
          | J (w, _) -> w = world))
    assumptions

(* printers *)
let logic_symbols


let rec pp_print_theorem fmtr th =
  let print_theorems theorems name =
    pp_print_string fmtr name;
    List.iter
      (function
        | th ->
            pp_print_cut fmtr ();
            pp_print_theorem fmtr th)
      theorems
  in
  pp_open_vbox fmtr 1;
  (match th with
  | FalseE (th, _) -> print_theorems [ th ] "FalseE"
  | Hyp _ -> print_theorems [] "Hyp"
  | ConI (th1, th2, _) -> print_theorems [ th1; th2 ] "ConI"
  | ConE (th, _) -> print_theorems [ th ] "ConE"
  | AltI (th, _) -> print_theorems [ th ] "AltI"
  | AltE (th1, th2, th3, _) -> print_theorems [ th1; th2; th3 ] "AltE"
  | ImpI (th, _) -> print_theorems [ th ] "ImpI"
  | ImpE (th1, th2, _) -> print_theorems [ th1; th2 ] "ImpE"
  | BoxI (th, _) -> print_theorems [ th ] "BoxI"
  | BoxE (th1, th2, _) -> print_theorems [ th1; th2 ] "BoxE"
  | DiaI (th1, th2, _) -> print_theorems [ th1; th2 ] "DiaI"
  | DiaE (th1, th2, _) -> print_theorems [ th1; th2 ] "ImpE"
  | D (th, _) -> print_theorems [ th ] "RD"
  | T (th, _) -> print_theorems [ th ] "RT"
  | B (th1, th2, _) -> print_theorems [ th1; th2 ] "RB"
  | Four (th1, th2, th3, _) -> print_theorems [ th1; th2; th3 ] "Four"
  | Five (th1, th2, th3, _) -> print_theorems [ th1; th2; th3 ] "Five"
  | Two (th1, th2, th3, _) -> print_theorems [ th1; th2; th3 ] "Two");
  pp_print_cut fmtr ();
  pp_close_box fmtr ();
  let r, ass, jgmt = destruct_th th in
  pp_open_hbox fmtr ();
  pp_print_assumptions fmtr th;
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_judgement fmtr ~r jgmt;
  pp_close_box fmtr ()

and pp_print_assumptions fmtr th =
  let r, ass, _ = destruct_th th in
  pp_print_string fmtr r;
  pp_print_string fmtr " :: ";
  if ass = [] then pp_print_string fmtr "•"
  else (
    pp_open_hvbox fmtr 0;
    List.iter
      (function
        | a ->
            pp_print_judgement fmtr ~r a;
            pp_print_string fmtr ";";
            pp_print_space fmtr ())
      ass;
    pp_close_box fmtr ())

and pp_print_judgement fmtr ?r = function
  | R (x, y) ->
      let name =
        match r with
        | None -> failwith "relation needed to print judgement"
        | Some r -> r
      in

      pp_print_string fmtr x;
      pp_print_string fmtr name;
      pp_print_string fmtr y
  | J (world, p) ->
      pp_open_hbox fmtr ();
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

let print_theorem = pp_print_theorem std_formatter
let print_judgement = pp_print_judgement std_formatter
let print_prop = pp_print_imp_prop std_formatter
