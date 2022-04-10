open Syntax


type rel_properties =
  | Seriality
  | Reflexivity
  | Symmetry
  | Transitivity
  | Euclideanness
  | Directedness

type statement =
  | Rel of string * rel_properties list
  | Theorem of string (* name of theorem*)
              * string (* name of relation *)
              * judgement 
  | Proof
  