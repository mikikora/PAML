open Syntax

type location = {loc_file : string; loc_char : int; loc_line : int}
type 'a located = {v : 'a; l : location}

type command =
  | IntroCmd of string option * world option
  | ApplyCmd of string option * string option * world option * judgement
  | ApplyAssmCmd of string
  | SplitCmd
  | LeftCmd
  | RightCmd
  | AbandonCmd
  | QedCmd
  | ProofCmd

type rel_properties =
  | Seriality
  | Reflexivity
  | Symmetry
  | Transitivity
  | Euclideanness
  | Directedness

type statement_raw =
  | RelDecl of string * rel_properties list
  | RelProperties of string * rel_properties list
  | RelRmProperties of string * rel_properties list
  | TheoremDecl of string * string * judgement
  | Command of command

and statement = statement_raw located