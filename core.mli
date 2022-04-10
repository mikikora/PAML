open Syntax

val hyp : assumptions -> judgement -> theorem
val coni : theorem -> theorem -> theorem
val cone1 : theorem -> theorem
val cone2 : theorem -> theorem
val alti1 : theorem -> prop -> theorem
val alti2 : theorem -> prop -> theorem
val alte : theorem -> theorem -> theorem -> theorem
val impi : judgement -> theorem -> theorem
val impe : theorem -> theorem -> theorem
val boxi : theorem -> world -> theorem
val boxe : theorem -> theorem -> world -> theorem
val diai : theorem -> theorem -> world -> theorem
val diae : theorem -> theorem -> world -> theorem
val falsee : judgement -> theorem -> theorem
