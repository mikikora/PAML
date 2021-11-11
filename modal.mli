(** Propositions *)
type proposition = P | Imp of proposition * proposition | Box of proposition | Dia of proposition

val pp_print_prop : Format.formatter -> proposition -> unit

type proof_term = Varx of string 
                  | Abs of string * proposition * proof_term
                  | App of proof_term * proof_term
                  | Boxp of proof_term
                  | LetBox of string * proof_term * proof_term
                  | Diap of proof_expr
and proof_expr = I of proof_term
                  | LetDiae of string * proof_term * proof_expr
                  | LetBoxe of string * proof_term * proof_expr

type context = C of (string * proposition) list 

type tru_jmnt = TJ of context * context * proof_term * proposition 

type pos_jmnt = PJ of context * context * proof_expr * proposition 

val pp_print_proof_term : Format.formatter -> proof_term -> unit

val pp_print_proof_expression : Format.formatter -> proof_expr -> unit

val pp_print_tru_jmnt : Format.formatter -> tru_jmnt -> unit

val pp_print_pos_jmnt : Format.formatter -> pos_jmnt -> unit

type theorem =
  | Hyp of tru_jmnt
  | ImpI of theorem * tru_jmnt
  | ImpE of theorem * theorem * tru_jmnt
  | HypS of tru_jmnt
  | BoxI of theorem * tru_jmnt
  | BoxE of theorem * theorem * tru_jmnt
  | BoxEp of theorem * theorem * pos_jmnt
  | DiaI of theorem * tru_jmnt
  | DiaE of theorem * theorem * pos_jmnt
  | PosI of theorem * pos_jmnt
  
val pp_print_theorem : Format.formatter -> theorem -> unit

val tru2pos : theorem -> theorem

val hyp : tru_jmnt -> theorem

val hyps : tru_jmnt -> theorem

val impi : theorem -> string -> theorem