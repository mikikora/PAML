open Proof_syntax
open Ast

val get_intro_hints : goal_desc -> command list
val get_apply_hints : goal_desc -> command list
val get_property_hints : goal_desc -> command list
val get_hints : goal_desc -> command list
val get_hint_string : goal_desc -> string
