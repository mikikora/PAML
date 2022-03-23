type location = Location of { line : int; column : int; file : string }

type 'a located = { v : 'a; loc : location }

val mkLocation : Lexing.position * Lexing.position -> location

type identifier = Identifier of string

type raw_statement =
  | Proof of string * form * env option * env option
  | Focus of int32 option
  | Intro of string option
  | Applyt of string
  | Applyv of string
  | Apply of form * string option
  | ApplyThm of string
  | Qed
  | Poss
  | Valid
  | FromTrue
  | Unfocus

and form = Var of string | Imp of form * form | Box of form | Dia of form

and env = form list

and statement = raw_statement located