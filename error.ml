open Ast

exception Error of string Ast.located
exception UnlocatedError of string

let report_error err =
  let loc = err.l in
  Printf.fprintf stderr "%s:%d:%d: %s" loc.loc_file loc.loc_line loc.loc_char
    err.v;
  flush stderr;
  print_newline ()
