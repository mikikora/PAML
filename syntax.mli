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
type judgement = J of world * prop | R of world * world
type assumptions = judgement list
type theorem_context = Relation.relation_name * assumptions * judgement

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
val relation : theorem -> string
val assumptions : theorem -> assumptions
val consequence : theorem -> judgement
val destruct_th : theorem -> theorem_context
val assumptions_with_world : world -> assumptions -> judgement list

(* printers *)
type printing_style = LaTeX | Backup | Interactive

val theorem_rule_to_string : ?style:printing_style -> theorem_rule -> string
val pp_print_theorem : ?style:printing_style -> formatter -> theorem -> unit
val pp_print_assumptions : ?style:printing_style -> formatter -> theorem -> unit

val pp_print_judgement :
  ?style:printing_style -> formatter -> ?r:string -> judgement -> unit

val print_theorem : theorem -> unit
val print_judgement : ?r:string -> judgement -> unit
val print_prop : prop -> unit
