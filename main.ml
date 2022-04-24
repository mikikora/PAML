open Syntax
open Ast
open Format
open Commands

exception ParseError of location

let inFile : string option ref = ref None
let outfile = "output.f"

let read_eval () =
  let lexbuf = Lexer.create_from_stdin () in
  let parser_result = Parser.statement Lexer.token lexbuf in
  Parsing.clear_parser ();
  parser_result

let rec interactive () =
  let statement =
    try read_eval ()
    with 
      | Lexer.Eof ->
        let () = print_endline "End" in
        exit 0
      | Parser.Error ->
        let loc = (Lexer.get_location ()) in
        Printf.fprintf stderr "%s:%d:%d: Parse error" loc.loc_file loc.loc_line loc.loc_char;
        flush stderr;
        exit 1
  in
  interpret_statement statement;
  let _ = Sys.command "clear" in 
  print_current_state ();
  print_flush ();
  interactive ()

let main () =
  let _ = Sys.command "clear" in 
  let () = print_endline "Start" in
  interactive ()

let () = main ()
