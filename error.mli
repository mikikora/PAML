exception Error of string Ast.located
exception UnlocatedError of string

val report_error : string Ast.located -> unit
