type proposition =
  | P
  | Var of string
  | Imp of proposition * proposition
  | Box of proposition
  | Dia of proposition

val pp_print_prop : Format.formatter -> proposition -> unit

type jmnt = True of proposition | Valid of proposition | Poss of proposition

val pp_print_jmnt : Format.formatter -> jmnt -> unit

type theorem =
  | Hyp of proposition list * proposition list * jmnt
  | ImpI of theorem * (proposition list * proposition list * jmnt)
  | ImpE of theorem * theorem * (proposition list * proposition list * jmnt)
  | HypS of proposition list * proposition list * jmnt
  | BoxI of theorem * (proposition list * proposition list * jmnt)
  | BoxE of theorem * (proposition list * proposition list * jmnt)
  | BoxEp of theorem * theorem * (proposition list * proposition list * jmnt)
  | DiaI of theorem * (proposition list * proposition list * jmnt)
  | DiaE of theorem * theorem * (proposition list * proposition list * jmnt)
  | PosI of theorem * (proposition list * proposition list * jmnt)

val assumption_valid : theorem -> proposition list

val assumption_true : theorem -> proposition list

val consequence : theorem -> jmnt

val pp_print_theorem : Format.formatter -> theorem -> unit

val posi : theorem -> theorem

val hyp : proposition list -> proposition list -> proposition -> theorem

val hyps : proposition list -> proposition list -> proposition -> theorem

val impi : theorem -> proposition -> theorem

val impe : theorem -> theorem -> theorem

val boxi : theorem -> proposition list -> theorem
