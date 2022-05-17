exception Eof
exception InvalidToken of string Ast.located * string

val create_from_file : in_channel -> string -> Lexing.lexbuf
val create_from_stdin : unit -> Lexing.lexbuf
val get_location : unit -> Ast.location
val locate : 'a -> 'a Ast.located
val token : Lexing.lexbuf -> Parser.token
