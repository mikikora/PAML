{
  (* Based on tapl - fullsimple *)
  open Parser
  open Lexing
  open Ast
  open Error

  type token = Parser.token

  exception InvalidToken of Ast.location * string

  let handleError lexbuf =
    let pos = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) in
    let token = Lexing.lexeme lexbuf in
    let exc = InvalidToken (mkLocation pos, token) in
    raise exc

  exception Eof

  let reserved_keywords = [
    (* keywords *)
    ("Rel", REL_KW);
    ("Seriality", PROPERITY Seriality);
    ("Ser", PROPERITY Seriality);
    ("Reflexivity", PROPERITY Reflexivity);
    ("Refl", PROPERITY Reflexivity);
    ("Symmetry", PROPERITY Symmetry);
    ("Symm", PROPERITY Symmetry);
    ("Transitivity", PROPERITY Transitivity);
    ("Trans", PROPERITY Transitivity);
    ("Euclideanness", PROPERITY Euclideanness);
    ("Eucl", PROPERITY Euclideanness);
    ("Directedness", PROPERITY Directedness);
    ("Direct", PROPERITY Directedness);
    ("Proof", PROOF);
    ("Theorem", THEOREM);
    ("And", AND);
    ("and", AND);
    ("Or", OR);
    ("or", OR);
    ("Box", BOX);
    ("box", BOX);
    ("dia", DIAMOND);
    ("Dia", DIAMON);
    ("diamond", DIAMOND);
    ("Diamond", DIAMOND);
    ("False", FALSE);
    ("F", FALSE)
  ]

  let (symbolTable : (string, Parser.token) Hashtbl.t) = Hashtbl.create 1024

  let () =
    List.iter (function (str, t) -> Hashtbl.add symbolTable str t) reservedWords
    
  let createID str =
    try 
      Hashtbl.find symbolTable str
    with _ ->
      ID str
}


let number = ['0' - '9']+
let identifier = ['_' 'a'-'z' 'A'-'Z']['_' 'A'-'Z' 'a'-'z' '0'-'9' ''']*

rule token = parse
  | ['\n']
  { new_line lexbuf; token lexbuf}

  | "//"
  { line_comment lexbuf }

  | eof 
  { raise Eof }

  | ['\t' ' ' '\r']
  { token lexbuf }

  | number ['a'-'z' 'A'-'Z' ''' '_'] 
  { handleError lexbuf }

  | "*/" 
  { error (info lexbuf) "Unmatched end of comment" }

  | "/*" 
  { startLex := info lexbuf; comment lexbuf; main lexbuf }

  | identifier
  {
    createID (text lexbuf)
  }

  | '.' { DOT }
  | ',' {COMMA}
  | ':' {COLON}
  | '(' {LPAR}
  | ')' {RPAR}
  | "->" {AROOW}

and line_comment = parse
  | '\n'
  { new_line lexbuf; token lexbuf }

  | eof
  { raise Eof }

  | _ 
  { line_comment lexbuf }

and comment = parse
  | "*/"
  { }
  
  | eof
  { error (!startLex) "Comment not terminated" }

  | '\n'
  { newline lexbuf; comment lexbuf }
  
  | _
  { comment lexbuf }