open Syntax
open Ast
open Format

let inFile : string option ref = ref None
let outfile = "output.f"

let read_eval () =
  let lexbuf = Lexer.create_from_stdin () in
  let parser_result =
    Parser.statement Lexer.token lexbuf in
  Parsing.clear_parser ();
  parser_result
    

let interactive () =
  let result = 
    try 
      read_eval ()
    with Lexer.Eof ->
      let () = print_endline "end" in
      exit 0
  in
  match result.v with
  | TheoremDecl (id, rel, jgmt) ->
    print_string id;
    print_space ();
    print_string rel;
    print_space ();
    print_judgement jgmt;
    print_newline ()
  | _ -> ()

let main () =
  let () = print_endline "start" in
  interactive ()

let () = main ()