open Syntax
open Ast
open Format
open Commands
open Error

exception ParseError of location

let inFile : string option ref = ref None
let outfile = "output.f"

let rec read_eval () =
  let lexbuf = Lexer.create_from_stdin () in
  let parser_result =
    try Parser.statement Lexer.token lexbuf with
    | Lexer.Eof ->
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
  let statement = read_eval () in
  let () =
    try
      interpret_statement statement;
      let _ = Sys.command "clear" in
      print_current_state ();
      print_flush ()
    with Error err -> report_error err
  in
  interactive ()

let main () =
  let _ = Sys.command "clear" in
  print_current_state ();
  interactive ()

let () = main ()
