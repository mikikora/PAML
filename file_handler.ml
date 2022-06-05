open Commands
open Format
open Relation
open Syntax
open Ast
open Error

let pp_print_option fmtr a_fun = function
  | None -> pp_print_string fmtr "^None"
  | Some a ->
      pp_print_string fmtr "^Some ";
      a_fun fmtr a

let pp_print_assignments fmtr assignments =
  List.iter
    (function
      | name, prop ->
          pp_print_string fmtr name;
          pp_print_string fmtr ", ";
          pp_print_prop ~style:Backup fmtr prop)
    assignments

let rec pp_print_command fmtr = function
  | UndoCmd | AbandonCmd | QedCmd -> ()
  | IntroCmd (name, world) ->
      pp_print_string fmtr "^IntroCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string world
  | ApplyCmd (name1, name2, world, jgmt) ->
      pp_print_string fmtr "^ApplyCmd ";
      pp_print_option fmtr pp_print_string name1;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string name2;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string world;
      pp_print_string fmtr ", ";
      pp_print_judgement ~style:Backup fmtr jgmt
  | ApplyAssmCmd (name1, name2, world, name) ->
      pp_print_string fmtr "^ApplyAssmCmd ";
      pp_print_option fmtr pp_print_string name1;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string name2;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string world;
      pp_print_string fmtr ", ";
      pp_print_string fmtr name
  | ApplyThCmd (name1, name2, world, name, assignments) ->
      pp_print_string fmtr "^ApplyThCmd ";
      pp_print_option fmtr pp_print_string name1;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string name2;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string world;
      pp_print_string fmtr ", ";
      pp_print_string fmtr name;
      pp_print_string fmtr ", ";
      pp_print_assignments fmtr assignments
  | SplitCmd -> pp_print_string fmtr "^SplitCmd "
  | LeftCmd -> pp_print_string fmtr "^LeftCmd "
  | RightCmd -> pp_print_string fmtr "^RightCmd "
  | SerialCmd (name, world1, world2) ->
      pp_print_string fmtr "^SerialCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_option fmtr pp_print_string world1;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world2
  | ReflCmd (name, world) ->
      pp_print_string fmtr "^ReflCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world
  | SymmCmd (name, world1, world2) ->
      pp_print_string fmtr "^SymmCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world1;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world2
  | TransCmd (name, world1, world2, world3) ->
      pp_print_string fmtr "^TransCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world1;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world2;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world3
  | EuclCmd (name, world1, world2, world3) ->
      pp_print_string fmtr "^EuclCmd ";
      pp_print_option fmtr pp_print_string name;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world1;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world2;
      pp_print_string fmtr ", ";
      pp_print_string fmtr world3
  | DirectCmd (name1, name2, world1, world2, world3, world4) ->
      pp_print_string fmtr "^DirectCmd ";
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
  | ProofCmd -> pp_print_string fmtr "^ProofCmd "
  | FocusCmd n ->
      pp_print_string fmtr "^FocusCmd ";
      pp_print_int fmtr n
  | UnfocusCmd -> pp_print_string fmtr "^UnfocusCmd "
  | ContraCmd world ->
      pp_print_string fmtr "^ContraCmd ";
      pp_print_string fmtr world
  | AssumptionCmd -> pp_print_string fmtr "^AssumptionCmd "
  | ChainCmd (cmd1, cmd2) ->
      pp_print_string fmtr "^ChainCmd ";
      pp_open_vbox fmtr 1;
      pp_print_newline fmtr ();
      pp_print_command fmtr cmd1;
      pp_print_newline fmtr ();
      pp_print_command fmtr cmd2;
      pp_close_box fmtr ()
  | TryCmd cmd ->
      pp_print_string fmtr "^TryCmd ";
      pp_print_command fmtr cmd
  | AutoCmd n ->
      pp_print_string fmtr "^AutoCmd ";
      pp_print_int fmtr n

let create_backup name =
  let ch_out =
    try open_out name
    with _ -> raise (Error.UnlocatedError "Failed to open specified file")
  in
  let fmtr = formatter_of_out_channel ch_out in
  Seq.iter
    (function
      | rel ->
          pp_print_relation fmtr rel;
          pp_print_newline fmtr ())
    (get_declared_relations ());

  pp_print_string fmtr ";;\n";

  Seq.iter
    (fun (name, th) ->
      pp_open_vbox fmtr 1;
      pp_print_string fmtr "{";
      pp_print_newline fmtr ();
      pp_print_string fmtr name;
      pp_print_newline fmtr ();
      pp_print_theorem ~style:Backup fmtr th;
      pp_close_box fmtr ();
      pp_print_newline fmtr ();
      pp_print_string fmtr "}\n")
    (get_proven_theorems ());

  pp_print_string fmtr ";;\n";

  let theorem_to_prove, proof_command_list = get_current_proof_for_backup () in
  (match theorem_to_prove with
  | Some (n, r, jgmt) ->
      pp_print_string fmtr n;
      pp_print_newline fmtr ();
      pp_print_string fmtr r;
      pp_print_newline fmtr ();
      pp_print_judgement ~style:Backup fmtr jgmt;
      pp_print_newline fmtr ();
      pp_print_newline fmtr ();
      List.iter
        (function
          | elem ->
              pp_print_command fmtr elem;
              pp_print_newline fmtr ())
        proof_command_list
  | None -> ());
  pp_print_string fmtr ";;\n";
  pp_print_flush fmtr ();
  close_out ch_out

let load_backup name =
  let ch_in =
    try open_in name with _ -> raise (UnlocatedError "Failed to open file")
  in
  let lexbuf = Lexer.create_from_file ch_in name in
  let () =
    try
      let relation_list, theorem_list, theorem_to_prove =
        Parser.backup Lexer.token lexbuf
      in
      List.iter
        (function rel -> add_new_relation rel.name rel.properties)
        relation_list;
      List.iter
        (function
          | name, th ->
              if Core.validate_theorem th && assumptions th = [] then
                add_theorem_from_backup name th
              else raise (UnlocatedError ("Theorem " ^ name ^ " isn't true")))
        theorem_list;
      match theorem_to_prove with
      | None -> ()
      | Some (name, rel, jgmt, cmd_lst) ->
          interpret_statement
            (Lexer.locate (TheoremDecl (name, Some rel, jgmt)));
          List.iter
            (function
              | cmd -> interpret_statement @@ Lexer.locate (Command cmd))
            cmd_lst
    with
    | Lexer.Eof -> raise (UnlocatedError "Unexpected EOF")
    | Parser.Error ->
        let l = Lexer.get_location () in
        raise (Error { v = "Parse fds error"; l })
    | Lexer.InvalidToken (ch, msg) ->
        let l = ch.l in
        raise (Error { v = msg; l })
  in
  Parsing.clear_parser ();
  close_in ch_in

let rec print_tree fmtr th =
  let print_judgement_with_assumptions jgmt =
    pp_print_assumptions ~style:LaTeX fmtr th;
    pp_print_string fmtr " \\vdash ";
    pp_print_judgement ~style:LaTeX fmtr ~r:(relation th) jgmt;
    pp_print_string fmtr "$}";
    pp_print_newline fmtr ()
  in
  match th with
  | Assumption (rule, (_, _, jgmt)) ->
      pp_print_string fmtr
        ("\\AxiomC{}\n\\RightLabel{\\scriptsize$"
        ^ theorem_rule_to_string ~style:LaTeX rule
        ^ "$}\n");
      pp_print_string fmtr "\\UnaryInfC{$";
      print_judgement_with_assumptions jgmt
  | Single (rule, th1, (_, _, jgmt)) ->
      print_tree fmtr th1;
      pp_print_string fmtr
        ("\\RightLabel{\\scriptsize$"
        ^ theorem_rule_to_string ~style:LaTeX rule
        ^ "$}\n");
      pp_print_string fmtr "\\UnaryInfC{$";
      print_judgement_with_assumptions jgmt
  | Double (rule, th1, th2, (_, _, jgmt)) ->
      print_tree fmtr th1;
      print_tree fmtr th2;
      pp_print_string fmtr
        ("\\RightLabel{\\scriptsize$"
        ^ theorem_rule_to_string ~style:LaTeX rule
        ^ "$}\n");
      pp_print_string fmtr "\\BinaryInfC{$";
      print_judgement_with_assumptions jgmt
  | Triple (rule, th1, th2, th3, (_, _, jgmt)) ->
      print_tree fmtr th1;
      print_tree fmtr th2;
      print_tree fmtr th3;
      pp_print_string fmtr
        ("\\RightLabel{\\scriptsize$"
        ^ theorem_rule_to_string ~style:LaTeX rule
        ^ "$}\n");
      pp_print_string fmtr "\\TrinaryInfC{$";
      print_judgement_with_assumptions jgmt

let create_latex name =
  let header =
    "\n\
     \\documentclass[a4paper,10pt]{article}\n\n\
     \\usepackage[hdivide={1.25cm,*,1.25cm},vdivide={2cm,*,1.5cm},landscape]{geometry}\n\
     \\usepackage{bussproofs}\n\
     \\usepackage{latexsym}\n\
     \\begin{document}\n"
  in
  let ch_out =
    try open_out name
    with _ -> raise (Error.UnlocatedError "Failed to open specified file")
  in
  let fmtr = formatter_of_out_channel ch_out in
  pp_print_string fmtr header;
  Seq.iter
    (function
      | name, theorem ->
          pp_print_string fmtr ("\\section{" ^ name ^ "}");
          pp_print_newline fmtr ();
          let rel = get_relation (relation theorem) in
          pp_print_string fmtr
            ("Relation " ^ rel.name ^ " has properties: "
            ^ List.fold_left
                (fun acc prop -> property_to_string prop ^ ", " ^ acc)
                "" rel.properties);
          pp_print_string fmtr "\\begin{prooftree}";
          print_tree fmtr theorem;
          pp_print_string fmtr "\\end{prooftree}")
    (get_proven_theorems ());
  pp_print_string fmtr "\n\\end{document}\n";
  pp_print_flush fmtr ();
  close_out ch_out
