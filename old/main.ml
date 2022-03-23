open Ast
open Proof
open Modal

let log_file = ref None

(* let channel_reader ch buf s n = *)

let handle_file = exit 0

let parse_decl lexbuf =
  let statement = Parser.statement Lexer.token lexbuf in
  statement

let read_eval env =
  (*let lexbuf = Lexing.from_function (channel_reader stdin (ref "")) in *)
  let lexbuf = Lexing.from_channel stdin in
  let d = parse_decl lexbuf in
  ()

let interactive env =
  try
    log_file := Some (open_out "session.modal");
    read_eval env
  with Lexer.Eof ->
    let () = print_endline "\nEnd" in
    exit 0

let main () =
  let env = [] in
  if Array.length Sys.argv == 1 then interactive env
  else ignore (handle_file env Sys.argv.(1))

let _ = main ()
