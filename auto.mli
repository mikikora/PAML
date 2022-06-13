val auto :
  (Ast.command -> Proof_syntax.goal -> Proof_syntax.goal) ->
  int ->
  Proof_syntax.goal ->
  Proof_syntax.goal

val fauto :
  (Ast.command -> Proof_syntax.goal -> Proof_syntax.goal) ->
  int ->
  Proof_syntax.goal ->
  Proof_syntax.goal
