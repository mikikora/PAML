open Syntax
open Format
open Relation

type context = (string * judgement) list
type goal_desc = relation_name * context * judgement

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

val world_in_context : world -> context -> bool
val add_to_ctx : ?name:string -> context -> judgement -> context
val no_goals : proof -> int
val goals : proof -> goal_desc list
val create_fresh_world_name : context -> world

(* printers *)
val pp_print_unfocused_proof : formatter -> proof -> unit
val pp_print_current_goal : formatter -> goal -> unit
val print_unfocused_proof : proof -> unit
val print_current_goal : goal -> unit
