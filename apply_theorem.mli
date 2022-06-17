open Syntax
open Proof_syntax

val apply_th :
  string option ->
  string option ->
  world option ->
  theorem ->
  (string * prop) list ->
  goal ->
  goal
