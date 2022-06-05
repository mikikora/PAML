val interpret_statement : Ast.statement -> unit
val print_outside_proof_mode : unit -> unit
val print_current_state : bool -> unit
val get_proven_theorems : unit -> (string * Syntax.theorem) Seq.t

val get_current_proof_for_backup :
  unit -> (string * string * Syntax.judgement) option * Ast.command list

val add_theorem_from_backup : string -> Syntax.theorem -> unit
