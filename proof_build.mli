open Syntax
open Core

type context = (string * judgement) list
type goal_desc = context * judgement

type proof =
  | Empty of goal_desc
  | Node1 of proof * (theorem -> theorem)
  | Node2 of proof * proof * (theorem -> theorem -> theorem)
  | Node3 of proof * proof * proof * (theorem -> theorem -> theorem -> theorem)
  | Leaf of theorem

type path =
  | Root
  | Left of path * proof * (theorem -> theorem -> theorem)
  | Right of path * proof * (theorem -> theorem -> theorem)
  | Mid of path * (theorem -> theorem)
  (*
     In 3 node proofs always goes: left, mid, right (without the one from the constructor)
     Sof of example in Left3 it goes: mid, right and in Right3: left, mid
  *)
  | Left3 of path * proof * proof * (theorem -> theorem -> theorem -> theorem)
  | Mid3 of path * proof * proof * (theorem -> theorem -> theorem -> theorem)
  | Right3 of path * proof * proof * (theorem -> theorem -> theorem -> theorem)

type goal = proof * path

val qed : proof -> theorem
val no_goals : proof -> int
val goals : proof -> goal_desc list
val proof : context -> judgement -> goal
val goal_desc : goal -> goal_desc
val unfocus : goal -> proof