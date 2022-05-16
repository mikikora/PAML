val interpret_statement : Ast.statement -> unit
val print_current_state : unit -> unit
val get_proven_theorems : unit -> (string * Syntax.theorem) Seq.t

val get_current_proof_for_backup :
  unit -> string option * Syntax.judgement option * Ast.command list
