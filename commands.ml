open Ast
open Proof

type current_proof = P of Proof_syntax.goal | Nothing

let current_proof : ref current_proof = ref Nothing

let theorem_map : (string, Syntax.theorem) Hashtbl.t = Hashtbl.create 5

let interpret_command = function c -> () (* TODO *)

let interpret_statement = function 
  | RelDecl(name, properties) -> Relation.add_new_relation name properties
  | RelProperties(name, properties) -> Relation.add_properties name properties
  | RelRmProperties(name, properties) -> Relation.remove_properties name properties
  | TheoremDecl(name, rel, jgmt) ->
    current_proof := proof (Relation.get_relation rel) [] jgmt
  | Command c -> interpret_command c
