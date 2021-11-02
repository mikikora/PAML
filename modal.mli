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

type theorem = Hyp of tru_jmnt
              | ImpI of tru_jmnt * tru_jmnt
              | ImpE of tru_jmnt * tru_jmnt * tru_jmnt
              | HypS of tru_jmnt
              | BoxI of tru_jmnt * tru_jmnt
              | BoxE of tru_jmnt * tru_jmnt * tru_jmnt
              | BoxEp of tru_jmnt * pos_jmnt * pos_jmnt
              | DiaI of pos_jmnt * tru_jmnt
              | DiaE of tru_jmnt * pos_jmnt * pos_jmnt


val hyp : tru_jmnt -> theorem