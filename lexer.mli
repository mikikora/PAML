exception Eof

val create_from_file : string -> Lexing.lexbuf
val create_from_stdin : unit -> Lexing.lexbuf
val get_location : unit -> Ast.location
val locate : 'a -> 'a Ast.located

val token : Lexing.lexbuf -> Parser.token