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
type theorem_context = relation_name * assumptions * judgement

type theorem_rule =
  | FalseE
  | Hyp
  | ConI
  | ConE1
  | ConE2
  | AltI1
  | AltI2
  | AltE
  | ImpI
  | ImpE
  | BoxI
  | BoxE
  | DiaI
  | DiaE of world
  | D of world * world
  | T of world
  | B
  | Four
  | Five
  | Two of world
  | Weak

type theorem =
  | Assumption of theorem_rule * theorem_context
  | Single of theorem_rule * theorem * theorem_context
  | Double of theorem_rule * theorem * theorem * theorem_context
  | Triple of theorem_rule * theorem * theorem * theorem * theorem_context

(* supporting functions *)
let relation = function
  | Assumption (_, (r, _, _))
  | Single (_, _, (r, _, _))
  | Double (_, _, _, (r, _, _))
  | Triple (_, _, _, _, (r, _, _)) ->
      r

let assumptions = function
  | Assumption (_, (_, l, _))
  | Single (_, _, (_, l, _))
  | Double (_, _, _, (_, l, _))
  | Triple (_, _, _, _, (_, l, _)) ->
      l

let consequence = function
  | Assumption (_, (_, _, x))
  | Single (_, _, (_, _, x))
  | Double (_, _, _, (_, _, x))
  | Triple (_, _, _, _, (_, _, x)) ->
      x

let theorem_rule = function
  | Assumption (rule, _)
  | Single (rule, _, _)
  | Double (rule, _, _, _)
  | Triple (rule, _, _, _, _) ->
      rule

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
type printing_style = LaTeX | Backup | Interactive

let theorem_rule_to_string ?(style = Backup) = function
  | FalseE -> if style = LaTeX then "(\\bot E)" else "^FalseE"
  | Hyp -> if style = LaTeX then "(Ass)" else "^Hyp"
  | ConI -> if style = LaTeX then "(\\wedge I)" else "^ConI"
  | ConE1 -> if style = LaTeX then "(\\wedge E1)" else "^ConE1"
  | ConE2 -> if style = LaTeX then "(\\wedge E2)" else "^ConE2"
  | AltI1 -> if style = LaTeX then "(\\vee I1)" else "^AltI1"
  | AltI2 -> if style = LaTeX then "(\\vee I2)" else "^AltI2"
  | AltE -> if style = LaTeX then "(\\vee E)" else "^AltE"
  | ImpI -> if style = LaTeX then "(\\supset I)" else "^ImpI"
  | ImpE -> if style = LaTeX then "(\\supset E)" else "^ImpE"
  | BoxI -> if style = LaTeX then "(\\Box I)" else "^BoxI"
  | BoxE -> if style = LaTeX then "(\\Box E)" else "^BoxE"
  | DiaI -> if style = LaTeX then "(\\diamond I)" else "^DiaI"
  | DiaE x -> if style = LaTeX then "(\\diamond E)" else "^DiaE " ^ x
  | D (x, y) -> if style = LaTeX then "(R_D)" else "^RD " ^ x ^ " " ^ y
  | T x -> if style = LaTeX then "(R_T)" else "^RT " ^ x
  | B -> if style = LaTeX then "(R_B)" else "^RB"
  | Four -> if style = LaTeX then "(R_4)" else "^Four"
  | Five -> if style = LaTeX then "(R_5)" else "^Five"
  | Two x -> if style = LaTeX then "(R_2)" else "^Two " ^ x
  | Weak -> if style = LaTeX then "(W)" else "^Weak"

let rec pp_print_theorem ?(style = Interactive) fmtr th =
  let print_theorems name rule theorems =
    pp_print_string fmtr name;
    pp_print_cut fmtr ();
    pp_print_string fmtr (theorem_rule_to_string ~style rule);
    List.iter
      (function
        | th ->
            pp_print_cut fmtr ();
            pp_print_theorem ~style fmtr th)
      theorems
  in
  pp_open_vbox fmtr 1;
  (match th with
  | Assumption (rule, _) -> print_theorems "^Assumption " rule []
  | Single (rule, th1, _) -> print_theorems "^Single " rule [ th1 ]
  | Double (rule, th1, th2, _) -> print_theorems "^Double " rule [ th1; th2 ]
  | Triple (rule, th1, th2, th3, _) ->
      print_theorems "^Triple " rule [ th1; th2; th3 ]);
  pp_print_cut fmtr ();
  pp_close_box fmtr ();
  let r, ass, jgmt = destruct_th th in
  pp_open_hbox fmtr ();
  pp_print_assumptions ~style fmtr th;
  let vdash =
    match style with Interactive -> "⊢" | Backup -> "|-" | LaTeX -> "\\vdash "
  in
  pp_print_string fmtr vdash;
  pp_print_space fmtr ();
  pp_print_judgement ~style fmtr ~r jgmt;
  pp_close_box fmtr ()

and pp_print_assumptions ?(style = Interactive) fmtr th =
  let r, ass, _ = destruct_th th in
  if style = Backup then (
    pp_print_string fmtr r;
    pp_print_string fmtr " :: ");
  let bullet =
    match style with Interactive -> "•" | Backup -> "@" | LaTeX -> " "
  in
  if ass = [] then pp_print_string fmtr bullet
  else (
    pp_open_hvbox fmtr 0;
    List.iter
      (function
        | a ->
            pp_print_judgement ~style fmtr ~r a;
            pp_print_string fmtr ";";
            pp_print_space fmtr ())
      ass;
    pp_close_box fmtr ())

and pp_print_judgement ?(style = Interactive) fmtr ?r = function
  | R (x, y) ->
      let name =
        match r with
        | None -> failwith "relation needed to print judgement"
        | Some r -> r
      in

      pp_print_string fmtr x;
      if style = Backup then pp_print_space fmtr ();
      pp_print_string fmtr name;
      if style = Backup then pp_print_space fmtr ();
      pp_print_string fmtr y
  | J (world, p) ->
      pp_open_hbox fmtr ();
      pp_print_string fmtr world;
      pp_print_space fmtr ();
      pp_print_string fmtr "::";
      pp_print_space fmtr ();
      pp_print_imp_prop ~style fmtr p;
      pp_close_box fmtr ()

and pp_print_imp_prop ?(style = Interactive) fmtr = function
  | Imp (p1, p2) ->
      pp_print_alt_prop ~style fmtr p1;
      pp_print_space fmtr ();
      let impl =
        match style with
        | Interactive -> "⊃"
        | Backup -> "->"
        | LaTeX -> "\\supset "
      in
      pp_print_string fmtr impl;
      pp_print_space fmtr ();
      pp_print_imp_prop ~style fmtr p2
  | _ as p -> pp_print_alt_prop ~style fmtr p

and pp_print_alt_prop ?(style = Interactive) fmtr = function
  | Alt (p1, p2) ->
      pp_print_con_prop ~style fmtr p1;
      pp_print_space fmtr ();
      let vee =
        match style with
        | Interactive -> "∨"
        | Backup -> "\\/"
        | LaTeX -> "\\vee "
      in
      pp_print_string fmtr vee;
      pp_print_space fmtr ();
      pp_print_con_prop ~style fmtr p2
  | _ as p -> pp_print_con_prop ~style fmtr p

and pp_print_con_prop ?(style = Interactive) fmtr = function
  | Con (p1, p2) ->
      pp_print_atom_prop ~style fmtr p1;
      pp_print_space fmtr ();
      let wedge =
        match style with
        | Interactive -> "∧"
        | Backup -> "/\\"
        | LaTeX -> "\\wedge "
      in
      pp_print_string fmtr wedge;
      pp_print_space fmtr ();
      pp_print_atom_prop ~style fmtr p2
  | _ as p -> pp_print_atom_prop ~style fmtr p

and pp_print_atom_prop ?(style = Interactive) fmtr = function
  | F ->
      let f =
        match style with
        | Interactive -> "⊥"
        | Backup -> "_|_"
        | LaTeX -> "\\bot "
      in
      pp_print_string fmtr f
  | Var x -> pp_print_string fmtr x
  | Box p ->
      let box =
        match style with
        | Interactive -> "◻"
        | Backup -> "[]"
        | LaTeX -> "\\Box "
      in
      pp_print_string fmtr box;
      pp_print_atom_prop ~style fmtr p
  | Dia p ->
      let dia =
        match style with
        | Interactive -> "◇"
        | Backup -> "<>"
        | LaTeX -> "\\Diamond "
      in
      pp_print_string fmtr dia;
      pp_print_atom_prop ~style fmtr p
  | _ as p ->
      pp_open_hvbox fmtr 1;
      pp_print_string fmtr "(";
      pp_print_cut fmtr ();
      pp_print_imp_prop ~style fmtr p;
      pp_print_cut fmtr ();
      pp_print_string fmtr ")";
      pp_close_box fmtr ()

let print_theorem = pp_print_theorem std_formatter
let print_judgement = pp_print_judgement std_formatter
let pp_print_prop = pp_print_imp_prop
let print_prop = pp_print_prop std_formatter
