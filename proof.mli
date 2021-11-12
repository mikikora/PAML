open Modal

type context = (string * proposition) list

type goalDesc = context * context * jmnt

type proof =
  | Empty of goalDesc
  | Node1 of proof * (theorem -> theorem)
  | Node2 of proof * proof * (theorem -> theorem -> theorem)
  | Leaf of theorem

type path =
  | Root
  | Left of path * proof * (theorem -> theorem -> theorem)
  | Right of path * proof * (theorem -> theorem -> theorem)
  | Mid of path * (theorem -> theorem)

type goal = Goal of proof * path

val qed : proof -> theorem

val numGoals : proof -> int

val goals : proof -> goalDesc list

val proof : context -> context -> jmnt -> proof

val goal : goal -> goalDesc

val unfocus : goal -> proof

val next : goal -> goal

val focus : int -> proof -> goal
