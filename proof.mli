open Syntax
open Core
open Proof_syntax
open Relation

val qed : proof -> theorem
val proof : relation -> context -> judgement -> proof

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
(* val intro_box : ?name:string -> world -> goal -> goal *)
(* val intro_diamond : world -> goal -> goal *)
(* val apply_diamond : ?names:string * string -> world -> judgement -> goal -> goal *)
