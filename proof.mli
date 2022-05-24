open Syntax
open Core
open Proof_syntax
open Relation

val qed : proof -> theorem
val proof : string -> context -> judgement -> proof
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

val apply_assm :
  ?name1:string option ->
  ?name2:string option ->
  ?world:world option ->
  string ->
  goal ->
  goal

val apply_th : 
  ?name1:string option ->
  ?name2:string option ->
  ?world:world option ->
  theorem ->
  goal ->
  goal

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

val assumption : goal -> goal
val chain_tactic : (goal -> goal) -> (goal -> goal) -> goal -> goal
val try_tactic : (goal -> goal) -> goal -> goal
