open Syntax
open Relation

type location = { loc_file : string; loc_char : int; loc_line : int }
type 'a located = { v : 'a; l : location }

type command =
  | IntroCmd of string option * world option
  | ApplyCmd of string option * string option * world option * judgement
  | ApplyAssmCmd of string option * string option * world option * string
  | ApplyThCmd of
      string option
      * string option
      * world option
      * string
      * (string * prop) list
  | SplitCmd
  | LeftCmd
  | RightCmd
  | SerialCmd of string option * world option * world
  | ReflCmd of string option * world
  | SymmCmd of string option * world * world
  | TransCmd of string option * world * world * world
  | EuclCmd of string option * world * world * world
  | DirectCmd of
      string option * string option * world * world * world * world option
  | AbandonCmd
  | QedCmd
  | ProofCmd
  | FocusCmd of int
  | UnfocusCmd
  | ContraCmd of world
  | UndoCmd
  | AssumptionCmd
  | ChainCmd of command * command
  | TryCmd of command
  | AutoCmd of int

type statement_raw =
  | RelDecl of string * rel_property list
  | RelProperties of string * rel_property list
  | RelRmProperties of string * rel_property list
  | TheoremDecl of string * relation_name option * judgement
  | LoadBackup of string
  | SaveBackup of string
  | GenerateLatex of string
  | EnterModel of relation_name option * system
  | ExitModel
  | ToggleHints of bool
  | ShowCmd
  | Command of command

and statement = statement_raw located
