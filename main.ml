open Syntax
open Ast
open Format
open Commands
open Error
open File_handler

let print_hints : bool ref = ref false

exception SigInt

let () = Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> raise SigInt))

let clear_screen () =
  print_string "\o033c";
  print_flush ()

let rec read_eval () =
  let lexbuf = Lexer.create_from_stdin () in
  let parser_result =
    try Parser.statement Lexer.token lexbuf with
    | Lexer.Eof ->
        let () = create_backup "backup.bck" in
        let () = print_endline "End" in
        exit 0
    | Error.UnlocatedError _ | Parser.Error ->
        let loc = Lexer.get_location () in
        Printf.fprintf stderr "%s:%d:%d: Parse error" loc.loc_file loc.loc_line
          loc.loc_char;
        flush stderr;
        print_newline ();
        read_eval ()
    | Lexer.InvalidToken (ch, msg) ->
        let loc = ch.l in
        Printf.fprintf stderr "%s:%d:%d: %s: %s" loc.loc_file loc.loc_line
          loc.loc_char msg ch.v;
        flush stderr;
        print_newline ();
        read_eval ()
  in
  Parsing.clear_parser ();
  parser_result

let rec interactive () =
  let statement = read_eval () in
  (match statement.v with
  | LoadBackup n -> (
      if is_in_prove_mode () then
        report_error { v = "Can't use it while proving"; l = statement.l }
      else
        try
          load_backup n;
          clear_screen ();
          print_string "Loaded successfully";
          print_newline ();
          print_current_state !print_hints;
          print_flush ()
        with
        | Error err -> report_error err
        | UnlocatedError msg -> report_error { v = msg; l = statement.l })
  | SaveBackup n -> (
      try
        create_backup n;
        clear_screen ();
        print_string "Saved";
        print_newline ();
        print_current_state !print_hints;
        print_flush ()
      with UnlocatedError msg -> report_error { v = msg; l = statement.l })
  | GenerateLatex n -> (
      if is_in_prove_mode () then
        report_error { v = "Can't use it while proving"; l = statement.l }
      else
        try
          create_latex n;
          clear_screen ();
          print_string "Generated";
          print_newline ();
          print_current_state !print_hints;
          print_flush ()
        with UnlocatedError msg -> report_error { v = msg; l = statement.l })
  | ToggleHints b ->
      print_hints := b;
      clear_screen ();
      print_string "Done";
      print_newline ();
      print_current_state !print_hints;
      print_flush ()
  | ShowCmd ->
      clear_screen ();
      if is_in_prove_mode () then print_outside_proof_mode ();
      print_string (String.init 40 (fun n -> if n mod 2 = 1 then '<' else '>'));
      print_newline ();
      print_flush ();
      print_current_state !print_hints;
      print_flush ()
  | _ -> (
      try
        interpret_statement statement;
        clear_screen ();
        print_current_state !print_hints;
        print_flush ()
      with Error err -> report_error err));
  interactive ()

let rec run_interactive () =
  try interactive ()
  with SigInt ->
    print_string "^C\n";
    print_flush ();
    run_interactive ()

let main () =
  clear_screen ();
  print_string
    "Welcome to interactive prover for modal logics\n\
     To turn on hints use \"Hints on\"\n\
     To enter specific modal model type \"Model name_of_model\"\n\n";
  print_current_state !print_hints;
  run_interactive ()

let () = main ()
