open Syntax
open Core
open Proof_syntax
open Relation

val get_father : goal -> goal
val qed : proof -> theorem
val proof : string -> context -> judgement -> proof
val unfocus : goal -> proof
val focus : int -> proof -> goal

(* Rules for building proof *)
val intro : string option -> world option -> goal -> goal

val apply :
  string option -> string option -> world option -> judgement -> goal -> goal

val apply_assm :
  string option -> string option -> world option -> string -> goal -> goal

val apply_th :
  string option -> string option -> world option -> theorem -> (string * prop) list -> goal -> goal

val split : goal -> goal
val left : goal -> goal
val right : goal -> goal
val contra : world -> goal -> goal
val serial : string option -> world option -> world -> goal -> goal
val refl : string option -> world -> goal -> goal
val symm : string option -> world -> world -> goal -> goal
val trans : string option -> world -> world -> world -> goal -> goal
val eucl : string option -> world -> world -> world -> goal -> goal

val direct :
  string option ->
  string option ->
  world ->
  world ->
  world ->
  world option ->
  goal ->
  goal

val assumption : goal -> goal
val chain_tactic : (goal -> goal) -> (goal -> goal) -> goal -> goal
val try_tactic : (goal -> goal) -> goal -> goal
