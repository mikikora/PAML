open Ast

(* open Proof *)
(* open Modal *)

let log_file = ref None

(* let channel_reader ch buf s n = *)

let handle_file _ = exit 0

let parse_decl lexbuf =
  let statement = Parser.statement Lexer.token lexbuf in
  statement

let read_eval env =
  let lexbuf = Lexing.from_channel stdin in
  let d = parse_decl lexbuf in
  match d.v with
  | Proof (id, f, etrue, evalid) -> print_endline ("proof " ^ id)
  | Focus num -> print_endline num
  | Intro id -> print_endline id
  
  print_endline "end"

let interactive env =
  try
    log_file := Some (open_out "session.modal");
    read_eval env
  with Lexer.Eof ->
    let () = print_endline "\nEnd" in
    exit 0

let main () =
  if false then ()
  else
    let env = [] in
    let () = print_endline "start" in
    interactive env

(* if Array.length Sys.argv = 1 then interactive env *)
(* else ignore (handle_file env Sys.argv.(1)) *)

let _ = main ()
