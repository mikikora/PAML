open Syntax
open Relation

val hyp : string -> assumptions -> judgement -> theorem
val coni : theorem -> theorem -> theorem
val cone1 : theorem -> theorem
val cone2 : theorem -> theorem
val alti1 : prop -> theorem -> theorem
val alti2 : prop -> theorem -> theorem
val alte : theorem -> theorem -> theorem -> theorem
val impi : judgement -> theorem -> theorem
val impe : theorem -> theorem -> theorem
val boxi : world -> theorem -> theorem
val boxe : world -> theorem -> theorem -> theorem
val diai : world -> theorem -> theorem -> theorem
val diae : world -> theorem -> theorem -> theorem
val falsee : judgement -> theorem -> theorem
val seriality : world -> world -> theorem -> theorem
val reflexivity : world -> theorem -> theorem
val symmetry : theorem -> theorem -> theorem
val transitivity : theorem -> theorem -> theorem -> theorem
val euclideanness : theorem -> theorem -> theorem -> theorem
val directedness : world -> theorem -> theorem -> theorem -> theorem
