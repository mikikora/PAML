open Ast
open Proof
open Format
open Proof_syntax

type current_proof = P of proof | G of goal | Nothing

let current_proof : current_proof ref = ref Nothing
let current_proof_name : string option ref = ref None
let theorem_map : (string, Syntax.theorem) Hashtbl.t = Hashtbl.create 5

let interpret_command cmd = 
  let get_goal () =
    match !current_proof with
    | G gl -> gl
    | _ -> failwith "Not focused"
  in
  match cmd with
  | IntroCmd (name, world) ->
      current_proof := G (intro ~name ~world (get_goal ()))
  | ApplyCmd (name1, name2, world, jgmt) ->
      current_proof := G (apply ~name1 ~name2 ~world jgmt (get_goal ()))
  | ApplyAssmCmd name -> current_proof := P (apply_assm name (get_goal ()))
  | SplitCmd -> current_proof := G (split (get_goal ()))
  | LeftCmd -> current_proof := G (left (get_goal ()))
  | RightCmd -> current_proof := G (right (get_goal ()))
  | AbandonCmd ->
      current_proof := Nothing;
      current_proof_name := None
  | QedCmd ->
      let complete_theorem =
        match !current_proof with
        | P pf -> qed pf
        | G _ -> failwith "Proof is not complete"
        | Nothing -> failwith "No proof to complete"
      in
      Hashtbl.add theorem_map (Option.get !current_proof_name) complete_theorem;
      current_proof := Nothing;
      current_proof_name := None
  | ProofCmd -> (
      match !current_proof with
      | P (Empty goal_desc) -> current_proof := G (focus 1 (Empty goal_desc))
      | P _ | G _ -> failwith "Proof statement starts fresh proof"
      | Nothing -> failwith "Nothing to proof")
  | FocusCmd n -> (
      match !current_proof with
      | P pf -> current_proof := G (focus n pf)
      | G _ -> failwith "Can't focus while in active goal"
      | Nothing -> failwith "Nothing to focus to")
  | UnfocusCmd -> (
      match !current_proof with
      | G gl -> current_proof := P (unfocus gl)
      | P _ -> failwith "Already unfocused"
      | Nothing -> failwith "Nothing to unfocus from")

let interpret_statement statement =
  match statement.v with
  | RelDecl (name, properties) -> Relation.add_new_relation name properties
  | RelProperties (name, properties) -> Relation.add_properties name properties
  | RelRmProperties (name, properties) ->
      Relation.remove_properties name properties
  | TheoremDecl (name, rel, jgmt) ->
      current_proof := P (proof (Relation.get_relation rel) [] jgmt);
      current_proof_name := Some name
  | Command c -> interpret_command c

let print_current_state () =
  match !current_proof with
  | Nothing -> print_endline "You can start new proof now.";
  | P pf -> print_unfocused_proof pf; print_newline ()
  | G gl -> print_current_goal gl; print_newline ()
