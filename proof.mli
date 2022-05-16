open Syntax
open Core
open Proof_syntax
open Relation

val qed : proof -> theorem
val proof : string -> context -> judgement -> proof

(* val goal_desc : goal -> goal_desc *)
val unfocus : goal -> proof
val focus : int -> proof -> goal

(* Rules for building proof *)
val intro : ?name:string option -> ?world:world option -> goal -> goal

val apply :
  ?name1:string option ->
  ?name2:string option ->
  ?world:world option ->
  judgement ->
  goal ->
  goal

val apply_assm : string -> goal -> proof
val split : goal -> goal
val left : goal -> goal
val right : goal -> goal
val contra : world -> goal -> goal
val serial : ?name:string option -> ?world:world option -> world -> goal -> goal
val refl : ?name:string option -> world -> goal -> goal
val symm : ?name:string option -> world -> world -> goal -> goal
val trans : ?name:string option -> world -> world -> world -> goal -> goal
val eucl : ?name:string option -> world -> world -> world -> goal -> goal

val direct :
  ?name1:string option ->
  ?name2:string option ->
  world ->
  world ->
  world ->
  ?world:world option ->
  goal ->
  goal

val assumption : goal -> proof
