open Commands
open Format
open Relation
open Syntax
open Ast

let pp_print_option fmtr a_fun = function
  | None -> pp_print_string fmtr "None"
  | Some a ->
      pp_print_string fmtr "Some ";
      a_fun fmtr a

let pp_print_command fmtr = function
  | UndoCmd | AbandonCmd | QedCmd -> ()
  | IntroCmd (name, world) ->
      pp_print_string fmtr "IntroCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string world
  | ApplyCmd (name1, name2, world, jgmt) ->
      pp_print_string fmtr "ApplyCmd ";
      pp_print_option fmtr pp_print_string name1;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string name2;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string world;
      pp_print_string fmtr ", ";
      pp_print_judgement fmtr jgmt
  | ApplyAssmCmd name ->
      pp_print_string fmtr "ApplyAssmCmd ";
      pp_print_string fmtr name
  | SplitCmd -> pp_print_string fmtr "SplitCmd "
  | LeftCmd -> pp_print_string fmtr "LeftCmd "
  | RightCmd -> pp_print_string fmtr "RightCmd "
  | SerialCmd (name, world1, world2) ->
      pp_print_string fmtr "SerialCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string world1;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world2
  | ReflCmd (name, world) ->
      pp_print_string fmtr "ReflCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world
  | SymmCmd (name, world1, world2) ->
      pp_print_string fmtr "SymmCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world1;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world2
  | TransCmd (name, world1, world2, world3) ->
      pp_print_string fmtr "TransCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world1;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world2;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world3
  | EuclCmd (name, world1, world2, world3) ->
      pp_print_string fmtr "EuclCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world1;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world2;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world3
  | DirectCmd (name1, name2, world1, world2, world3, world4) ->
      pp_print_string fmtr "DirectCmd ";
      pp_print_option fmtr pp_print_string name1;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string name2;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world1;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world2;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world3;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string world4
  | ProofCmd -> pp_print_string fmtr "ProofCmd "
  | FocusCmd n ->
      pp_print_string fmtr "FocusCmd ";
      pp_print_int fmtr n
  | UnfocusCmd -> pp_print_string fmtr "UnfocusCmd "
  | ContraCmd world ->
      pp_print_string fmtr "ContraCmd ";
      pp_print_string fmtr world
  | AssumptionCmd -> pp_print_string fmtr "AssumptionCmd "

let create_backup (backup_command : string Ast.located) =
  let ch_out =
    try open_out backup_command.v
    with _ ->
      raise
        (Error.Error
           { v = "Failed to open specified file"; l = backup_command.l })
  in
  let formatter = formatter_of_out_channel ch_out in
  Seq.iter
    (function
      | rel ->
          pp_print_relation formatter rel;
          pp_print_newline formatter ())
    (get_declared_relations ());

  pp_print_string formatter ";;\n";

  Seq.iter
    (fun (name, th) ->
      pp_open_vbox formatter 1;
      pp_print_string formatter "{";
      pp_print_newline formatter ();
      pp_print_string formatter name;
      pp_print_newline formatter ();
      pp_print_string formatter (relation th).name;
      pp_print_newline formatter ();
      pp_print_theorem formatter th;
      pp_close_box formatter ();
      pp_print_newline formatter ();
      pp_print_string formatter "}\n")
    (get_proven_theorems ());

  pp_print_string formatter ";;\n";

  let proof_name, proof_jgmt, proof_command_list =
    get_current_proof_for_backup ()
  in
  (match (proof_name, proof_jgmt) with
  | Some n, Some jgmt ->
      pp_print_string formatter n;
      pp_print_newline formatter ();
      pp_print_judgement formatter jgmt;
      pp_print_newline formatter ();
      List.iter
        (function
          | elem ->
              pp_print_command formatter elem;
              pp_print_newline formatter ())
        proof_command_list
  | None, None -> ()
  | _ -> failwith "Absurd");
  pp_print_flush formatter ();
  close_out ch_out

let load_backup name = 0
let create_latex ?name relations theorems = 0
