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
val relation : theorem -> string
val assumptions : theorem -> assumptions
val consequence : theorem -> judgement
val destruct_th : theorem -> theorem_context
val assumptions_with_world : world -> assumptions -> judgement list

(* printers *)
val pp_print_theorem : ?backup:bool -> formatter -> theorem -> unit

val pp_print_judgement :
  ?backup:bool -> formatter -> ?r:string -> judgement -> unit

val print_theorem : theorem -> unit
val print_judgement : ?r:string -> judgement -> unit
val print_prop : prop -> unit
