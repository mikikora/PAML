{
  (* Based on tapl - fullsimple *)
  open Parser
  open Lexing
  open Ast
  open Relation

  exception Eof

  let global_lexbuf = ref @@ Lexing.from_channel stdin

  let create stream =
    let lex = Lexing.from_channel stream in
    global_lexbuf := lex; lex
  
  let create_from_file in_file =
    let stream = open_in in_file in 
    create stream

  let create_from_stdin () =
    create stdin

  let get_location () =
    let pos = Lexing.lexeme_start_p !global_lexbuf in
    let fn = pos.pos_fname 
    and l = pos.pos_lnum
    and c = pos.pos_cnum - pos.pos_bol in
    {loc_file=fn; loc_char=c; loc_line=l}

  let locate x = 
    let loc = get_location () in
    {v=x; l=loc}

  exception InvalidToken of string Ast.located * string

  let handleError lexbuf msg =
    let token = Lexing.lexeme lexbuf in
    let exc = InvalidToken (locate token, msg) in
    raise exc


  let reserved_keywords = 
    List.map 
      (fun (str, tkn) -> String.lowercase_ascii str, tkn) 
      [
        (* keywords *)
        ("Relation", RELATION);
        ("Seriality", SERIAL);
        ("Ser", SERIAL);
        ("Reflexivity", REFL);
        ("Refl", REFL);
        ("Symmetry", SYMM);
        ("Symm", SYMM);
        ("Transitivity", TRANS);
        ("Trans", TRANS);
        ("Euclideanness", EUCL);
        ("Eucl", EUCL);
        ("Directedness", DIRECT);
        ("Direct", DIRECT);
        ("Proof", PROOF);
        ("Theorem", THEOREM);
        ("and", AND);
        ("or", OR);
        ("box", BOX);
        ("dia", DIA);
        ("diamond", DIA);
        ("False", FALSE);
        ("F", FALSE);
        ("as", AS);
        ("split", SPLIT);
        ("left", LEFT);
        ("right", RIGHT);
        ("apply", APPLY);
        ("intro", INTRO);
        ("with", WITH);
        ("unfocus", UNFOCUS);
        ("focus", FOCUS);
        ("qed", QED);
        ("abandon", ABANDON);
        ("contra", CONTRA);
        ("undo", UNDO);
      ]

  let (symbolTable : (string, Parser.token) Hashtbl.t) = Hashtbl.create 1024

  let () =
    List.iter (function (str, t) -> Hashtbl.add symbolTable str t) reserved_keywords
    
  let createID str =
    try 
      Hashtbl.find symbolTable (String.lowercase_ascii str)
    with _ ->
      ID str
}

let identifier = ['_' 'a'-'z' 'A'-'Z']['_' 'A'-'Z' 'a'-'z' '0'-'9' ''']*
let number = ['0' - '9']+


rule token = parse
  | ['\n']
  { token lexbuf}

  | eof 
  { raise Eof }

  | ['\t' ' ' '\r']
  { token lexbuf }

  | "*)" 
  { handleError lexbuf "Unmatched end of comment"}

  | "(*" 
  { comment lexbuf }

  | number ['a'-'z' 'A'-'Z' ''' '_'] 
  { handleError lexbuf "Forbidden token"}

  | number as num
  { NUM (int_of_string num) }

  | identifier as id
  {
    createID id
  }

  | '.' { DOT }
  | ',' {COMMA}
  | ':' {COLON}

  | '('
  | '['
  { LPAR}

  | ')' 
  | ']'
  {RPAR}

  | "->" {IMPL}
  | "[]" {BOX}
  | "<>" {DIA}
  | "/\\" {AND}
  | "\\/" {OR}

  | _ 
  { handleError lexbuf "Forbidden character" }

and comment = parse
  | "*)"
  { token lexbuf }
  
  | eof
  { handleError lexbuf "Comment not terminated" }

  | '\n'
  { comment lexbuf }
  
  | _
  { comment lexbuf }