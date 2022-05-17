open Syntax
open Ast
open Format
open Commands
open Error
open File_handler

exception ParseError of location

let rec read_eval () =
  let lexbuf = Lexer.create_from_stdin () in
  let parser_result =
    try Parser.statement Lexer.token lexbuf with
    | Lexer.Eof ->
        let () = create_backup "backup.bck" in
        let () = print_endline "End" in
        exit 0
    | Parser.Error ->
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
  let _ = Sys.command "clear" in
  print_current_state ();
  print_flush ();
  let statement = read_eval () in
  (match statement.v with
  | LoadBackup n -> (
      try load_backup n with
      | Error err -> report_error err
      | UnlocatedError msg -> report_error { v = msg; l = statement.l })
  | SaveBackup n -> (
      try create_backup n
      with UnlocatedError msg -> report_error { v = msg; l = statement.l })
  | _ -> (
      try interpret_statement statement with Error err -> report_error err));
  interactive ()

let main () =
  let _ = Sys.command "clear" in
  print_current_state ();
  interactive ()

let () = main ()
