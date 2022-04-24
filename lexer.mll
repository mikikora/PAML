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
    and c = pos.pos_bol in
    {loc_file=fn; loc_char=c; loc_line=l}

  let locate x = 
    let loc = get_location () in
    {v=x; l=loc}

  exception InvalidToken of string Ast.located * string

  let handleError lexbuf msg =
    let token = Lexing.lexeme lexbuf in
    let exc = InvalidToken (locate token, msg) in
    raise exc


  let reserved_keywords = [
    (* keywords *)
    ("Relation", RELATION);
    ("Seriality", PROPERTY Seriality);
    ("Ser", PROPERTY Seriality);
    ("Reflexivity", PROPERTY Reflexivity);
    ("Refl", PROPERTY Reflexivity);
    ("Symmetry", PROPERTY Symmetry);
    ("Symm", PROPERTY Symmetry);
    ("Transitivity", PROPERTY Transitivity);
    ("Trans", PROPERTY Transitivity);
    ("Euclideanness", PROPERTY Euclideanness);
    ("Eucl", PROPERTY Euclideanness);
    ("Directedness", PROPERTY Directedness);
    ("Direct", PROPERTY Directedness);
    ("Proof", PROOF);
    ("Theorem", THEOREM);
    ("And", AND);
    ("and", AND);
    ("Or", OR);
    ("or", OR);
    ("Box", BOX);
    ("box", BOX);
    ("dia", DIA);
    ("Dia", DIA);
    ("diamond", DIA);
    ("Diamond", DIA);
    ("False", FALSE);
    ("F", FALSE);
    ("as", AS);
    ("As", AS);
    ("split", SPLIT);
    ("Split", SPLIT);
    ("left", LEFT);
    ("Left", LEFT);
    ("Right", RIGHT);
    ("right", RIGHT);
    ("apply", APPLY);
    ("Apply", APPLY);
    ("apply_assm", APPLY_ASSM);
    ("Apply_assm", APPLY_ASSM);
    ("Apply_assumption", APPLY_ASSM);
    ("Intro", INTRO);
    ("intro", INTRO);
    ("with", WITH);
    ("unfocus", UNFOCUS);
    ("Unfocus", UNFOCUS);
    ("focus", FOCUS);
    ("Focus", FOCUS);
    ("qed", QED);
    ("Qed", QED);
    ("QED", QED)
  ]

  let (symbolTable : (string, Parser.token) Hashtbl.t) = Hashtbl.create 1024

  let () =
    List.iter (function (str, t) -> Hashtbl.add symbolTable str t) reserved_keywords
    
  let createID str =
    try 
      Hashtbl.find symbolTable str
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