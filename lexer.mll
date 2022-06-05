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
  
  let create_from_file in_stream name =
    let lexbuf = create in_stream in
    set_filename lexbuf name;
    lexbuf

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
        ("Serial", SERIAL);
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
        ("True", TRUE);
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
        ("unset", UNSET);
        ("Assumption", ASS);
        ("Load", LOAD);
        ("Save", SAVE);
        ("Generate", LATEX);
        ("try", TRY);
        ("auto", AUTO);
        ("exit", EXIT);
        ("model", MODEL);
        ("hints", HINT);
        ("on", ONOFF true);
        ("off", ONOFF false);
        ("show", SHOW);
        ("where", WHERE)
      ]

  let backup_reserved_keywords = [
    ("Assumption", ASSUMPTION);
    ("Single", SINGLE);
    ("Double", DOUBLE);
    ("Triple", TRIPLE);
    ("FalsE", RULE FalseE);
    ("Hyp", RULE Hyp);
    ("ConI", RULE ConI);
    ("ConE1", RULE ConE1);
    ("ConE2", RULE ConE2);
    ("AltI1", RULE AltI1);
    ("AltI2", RULE AltI2);
    ("AltE", RULE AltE);
    ("ImpI", RULE ImpI);
    ("ImpE", RULE ImpE);
    ("BoxI", RULE BoxI);
    ("BoxE", RULE BoxE);
    ("DiaI", RULE DiaI);
    ("DiaE", DIAE);
    ("RD", RD);
    ("RT", RT);
    ("RB", RULE B);
    ("Four", RULE Four);
    ("Five", RULE Five);
    ("Two", TWO);
    ("Weak", RULE Weak);
    ("IntroCmd", INTROCMD);
    ("ApplyCmd", APPLYCMD);
    ("ApplyAssmCmd", APPLYASSMCMD);
    ("ApplyThCmd", APPLYTHCMD);
    ("SplitCmd", SPLITCMD);
    ("LeftCmd", LEFTCMD);
    ("RightCmd", RIGHTCMD);
    ("SerialCmd", SERIALCMD);
    ("ReflCmd", REFLCMD);
    ("SymmCmd", SYMMCMD);
    ("TransCmd", TRANSCMD);
    ("EuclCmd", EUCLCMD);
    ("DirectCmd", DIRECTCMD);
    ("ProofCmd", PROOFCMD);
    ("FocusCmd", FOCUSCMD);
    ("UnfocusCmd", UNFOCUSCMD);
    ("ContraCmd", CONTRACMD);
    ("AssumptionCmd", ASSUMPTIONCMD);
    ("ChainCmd", CHAINCMD);
    ("TryCmd", TRYCMD);
    ("AutoCmd", AUTOCMD);
    ("None", NONE);
    ("Some", SOME);
  ]

  let (symbolTable : (string, Parser.token) Hashtbl.t) = Hashtbl.create 1024

  let () =
    List.iter (function (str, t) -> Hashtbl.add symbolTable str t) reserved_keywords

  let () =
    List.iter (function (str, t) -> Hashtbl.add symbolTable str t) backup_reserved_keywords
    
  let createID str =
    match (Hashtbl.find_opt symbolTable str,
           Hashtbl.find_opt symbolTable (String.lowercase_ascii str)) with
    | Some token, _ 
    | None, Some token -> token
    | None, None -> ID str
}

let identifier = ['_' 'a'-'z' 'A'-'Z']['_' 'A'-'Z' 'a'-'z' '0'-'9' ''']*
let number = ['0' - '9']+
let file_name = identifier | identifier '.' identifier


rule token = parse
  | ['\n']
  { new_line lexbuf; token lexbuf}

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

  | '"' (file_name as name) '"'
  {FILE_NAME name}

  | '.' { DOT }
  | ',' {COMMA}
  | ':' {COLON}
  | ';' {SEMICOLON}
  | '{' {LBRACE}
  | '}' {RBRACE}
  | '~' {NEG}
  | "|-" {VDASH}
  | "_|_" {FALSE}
  | "^|^" {TRUE}
  | '@' {EMPTY_ASSMP}
  | '=' {EQUAL}

  | '('
  | '['
  { LPAR}

  | ')' 
  | ']'
  {RPAR}

  | "->" {IMPL}
  | "<->" {EQUIV}
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