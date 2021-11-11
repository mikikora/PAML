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

val assumption_valid : theorem -> jmnt list

val assumption_true : theorem -> jmnt list

val consequence : theorem -> jmnt

val pp_print_theorem : Format.formatter -> theorem -> unit

val posi : theorem -> theorem

val hyp : jmnt list -> jmnt list -> proposition -> theorem

val hyps : jmnt list -> jmnt list -> proposition -> theorem
