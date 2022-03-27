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
val assumptions : theorem -> assumptions
val consequence : theorem -> judgement
val destruct_th : theorem -> assumptions * judgement

(* printers *)
val pp_print_theorem : Format.formatter -> theorem -> unit
val print_theorem : theorem -> unit
