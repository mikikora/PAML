{
    open Parser
    open Lexing
    open Ast

    type token = Parser.token

    exception InvalidToken of Ast.location * string

    let handleError lexbuf =
        let pos = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) in
        let token = Lexing.lexeme lexbuf in
        let exc = InvalidToken (mkLocation pos, token) in
        raise exc

    exception Eof
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

    | number as num
    { try INT (Int32.of_string num)
      with _ -> handleError lexbuf }

    | '.'
    { DOT }

    | '('
    { LPAR }

    | ')'
    { RPAR }

    | "["
    { LBRACK }

    | "]"
    { RBRACK }

    | "{"
    { LBRACE }

    | "}"
    { RBRACE }

    | ";"
    { SEMICOLON }

    | '='
    { ASSIGN }

    | "qed"
    { QED }

    | "proof"
    { PROOF }

    | "where"
    { WHERE }

    | "box"
    { BOX }

    | "dia"
    { DIA }

    | "->"
    { ARROW }

    | "apply"
    { APPLY }

    | "intro"
    { INTRO }

    | "applyt"
    | "applyv" as tac
    { STRTACTIC tac }

    | "undia"
    | "unbox"
    | "fromtrue"
    | "unfocus" as tac
    { SMPLTACTIC tac }

    | "focus"
    { FOCUS }

    | "as"
    { AS }

    | "true"
    { TRUE }

    | "valid"
    { VALID }

    | "and"
    { AND }

    | identifier as id
    { ID id }

    | _ 
    { handleError lexbuf }

and line_comment = parse
    | '\n'
    { new_line lexbuf; token lexbuf }

    | eof
    { raise Eof }

    | _ 
    { line_comment lexbuf }


