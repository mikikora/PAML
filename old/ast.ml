type location = Location of { line : int; column : int; file : string }

let string_of_location (Location { line; column; file }) =
  Format.sprintf "%s:%u:%u" file line column

type 'a located = { v : 'a; loc : location }

let mkLocation (loc, _) =
  let line = loc.Lexing.pos_lnum in
  let column = loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1 in
  let file = loc.Lexing.pos_fname in
  Location { line; column; file }

type identifier = Identifier of string

module IdMap = Map.Make (struct
  type t = identifier

  let compare = compare
end)

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
