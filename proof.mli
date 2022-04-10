open Syntax
open Core
open Proof_syntax

val qed : proof -> theorem
val proof : context -> judgement -> goal
val goal_desc : goal -> goal_desc
val unfocus : goal -> proof
val focus : int -> proof -> goal

(* Rules for building proof *)
val intro : string -> goal -> goal
val apply : ?names:string * string -> judgement -> goal -> goal
val apply_assm : string -> goal -> proof
val split : goal -> goal
val left : goal -> goal
val right : goal -> goal
val intro_box : string -> world -> goal -> goal
val intro_diamond : world -> goal -> goal
val apply_diamond : string -> string -> world -> judgement -> goal -> goal
