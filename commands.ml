open Ast
open Proof
open Format
open Proof_syntax
open Error

type current_proof = P of proof | G of goal

let current_proof : current_proof list ref = ref []
let current_proof_name : string option ref = ref None
let theorem_map : (string, Syntax.theorem) Hashtbl.t = Hashtbl.create 5

let interpret_command cmd =
  let get_goal () =
    match List.hd !current_proof with
    | G gl -> gl
    | _ -> raise (UnlocatedError "Not focused")
  in
  match cmd with
  | IntroCmd (name, world) ->
      current_proof := G (intro ~name ~world (get_goal ())) :: !current_proof
  | ApplyCmd (name1, name2, world, jgmt) ->
      current_proof :=
        G (apply ~name1 ~name2 ~world jgmt (get_goal ())) :: !current_proof
  | ApplyAssmCmd name ->
      current_proof := P (apply_assm name (get_goal ())) :: !current_proof
  | SplitCmd -> current_proof := G (split (get_goal ())) :: !current_proof
  | LeftCmd -> current_proof := G (left (get_goal ())) :: !current_proof
  | RightCmd -> current_proof := G (right (get_goal ())) :: !current_proof
  | ContraCmd w -> current_proof := G (contra w (get_goal ())) :: !current_proof
  | SerialCmd (name, world, x) ->
      current_proof := G (serial ~name ~world x (get_goal ())) :: !current_proof
  | ReflCmd (name, world) ->
      current_proof := G (refl ~name world (get_goal ())) :: !current_proof
  | SymmCmd (name, world1, world2) ->
      current_proof :=
        G (symm ~name world1 world2 (get_goal ())) :: !current_proof
  | TransCmd (name, world1, world2, world3) ->
      current_proof :=
        G (trans ~name world1 world2 world3 (get_goal ())) :: !current_proof
  | EuclCmd (name, world1, world2, world3) ->
      current_proof :=
        G (eucl ~name world1 world2 world3 (get_goal ())) :: !current_proof
  | DirectCmd (name1, name2, world1, world2, world3, world) ->
      current_proof :=
        G (direct ~name1 ~name2 world1 world2 world3 ~world (get_goal ()))
        :: !current_proof
  | AbandonCmd ->
      current_proof := [];
      current_proof_name := None
  | QedCmd ->
      let cur_th =
        try List.hd !current_proof
        with Failure _ -> raise (UnlocatedError "No proof to complete")
      in
      let complete_theorem =
        match cur_th with
        | P pf -> qed pf
        | G _ -> raise (UnlocatedError "Proof is not complete")
      in
      Hashtbl.add theorem_map (Option.get !current_proof_name) complete_theorem;
      current_proof := [];
      current_proof_name := None
  | ProofCmd -> (
      try
        match List.hd !current_proof with
        | P (Empty goal_desc) ->
            current_proof := G (focus 1 (Empty goal_desc)) :: !current_proof
        | P _ | G _ ->
            raise (UnlocatedError "Proof statement starts fresh proof")
      with Failure _ -> raise (UnlocatedError "Nothing to proof"))
  | FocusCmd n -> (
      try
        match List.hd !current_proof with
        | P pf -> current_proof := G (focus n pf) :: !current_proof
        | G _ -> raise (UnlocatedError "Can't focus while in active goal")
      with Failure _ -> raise (UnlocatedError "Nothing to focus to"))
  | UnfocusCmd -> (
      try
        match List.hd !current_proof with
        | G gl -> current_proof := P (unfocus gl) :: !current_proof
        | P _ -> raise (UnlocatedError "Already unfocused")
      with Failure _ -> raise (UnlocatedError "Nothing to unfocus from"))
  | UndoCmd -> (
      try current_proof := List.tl !current_proof with Failure _ -> ())

let interpret_statement statement =
  match !current_proof with
  | [] -> (
      try
        match statement.v with
        | RelDecl (name, properties) ->
            Relation.add_new_relation name properties
        | RelProperties (name, properties) ->
            Relation.add_properties name properties
        | RelRmProperties (name, properties) ->
            Relation.remove_properties name properties
        | TheoremDecl (name, rel, jgmt) ->
            current_proof := [ P (proof (Relation.get_relation rel) [] jgmt) ];
            current_proof_name := Some name
        | _ ->
            raise
              (Error
                 { v = "Can't use that outsite proof mode"; l = statement.l })
      with
      | Relation.RelationDoesNotExist msg ->
          raise (Error { v = msg; l = statement.l })
      | Relation.Error (name, msg) ->
          let new_msg = name ^ " : " ^ msg in
          raise (Error { v = new_msg; l = statement.l }))
  | _ -> (
      match statement.v with
      | Command c -> (
          try interpret_command c
          with UnlocatedError msg ->
            let loc = statement.l in
            raise (Error { v = msg; l = loc }))
      | _ ->
          raise (Error { v = "Can't use that in open proof"; l = statement.l }))

let print_current_state () =
  let print_command_prompt () =
    print_newline ();
    print_string (String.make 40 '=');
    print_newline ();
    print_string ">";
    print_flush ()
  in
  try
    match List.hd !current_proof with
    | P pf ->
        print_unfocused_proof pf;
        print_command_prompt ()
    | G gl ->
        print_current_goal gl;
        print_command_prompt ()
  with Failure _ ->
    open_vbox 0;
    print_string "Declared relations:";
    print_cut ();
    Seq.iter
      (fun (relation : Relation.relation) ->
        print_string relation.name;
        if relation.properties = [] then () else print_string " : ";
        open_hbox ();
        List.iter
          (fun (property : Relation.rel_properties) ->
            print_string @@ Relation.property_to_string property;
            print_string ",";
            print_space ())
          relation.properties;
        close_box ();
        print_cut ())
      (Relation.get_declared_relations ());
    print_string (String.make 40 '-');
    print_cut ();
    print_string "Complited theorems:";
    print_cut ();
    Hashtbl.iter
      (fun name th ->
        print_string (name ^ " [" ^ (Syntax.relation th).name ^ "]");
        print_cut ())
      theorem_map;
    close_box ();
    print_command_prompt ()
