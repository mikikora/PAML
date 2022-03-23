
(* The type of tokens. *)

type token = 
  | WHERE
  | VALID
  | TRUE
  | STRTACTIC of (string)
  | SMPLTACTIC of (string)
  | SEMICOLON
  | RPAR
  | RBRACK
  | RBRACE
  | QED
  | PROOF
  | LPAR
  | LBRACK
  | LBRACE
  | INTRO
  | INT of (int32)
  | ID of (string)
  | FOCUS
  | DOT
  | DIA
  | BOX
  | ASSIGN
  | AS
  | ARROW
  | APPLY
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val statement: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.statement)
