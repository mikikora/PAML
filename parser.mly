%start statement

%{
    open Ast

    let locate loc x = {loc = mkLocation loc; v=x}
%}


%type <Ast.statement> statement

%token <string>ID
%token QED
%token PROOF
%token WHERE
%token BOX
%token DIA
%token ARROW
%token APPLY
%token <string>SMPLTACTIC
%token <string>STRTACTIC
%token FOCUS
%token <int32>INT
%token AS
%token ASSIGN
%token DOT
%token LPAR
%token RPAR
%token LBRACK
%token RBRACK
%token LBRACE
%token RBRACE
%token TRUE
%token VALID
%token AND
%token INTRO
%token SEMICOLON

%left DIA BOX
%right ARROW

%%

statement:
    raw_statement DOT {locate $loc $1}

raw_statement:
    | PROOF id=ID ASSIGN f=form
    { Proof (id, f, None, None)}
    | PROOF id=ID ASSIGN f=form WHERE e=env TRUE
    { Proof (id, f, Some e, None)}
    | PROOF id=ID ASSIGN f=form WHERE e=env VALID
    { Proof (id, f, None, Some e)}
    | PROOF id=ID ASSIGN f=form WHERE e1=env TRUE AND e2=env VALID
    { Proof (id, f, Some e1, Some e2)}
    | FOCUS num=option(INT)
    { Focus num}
    | INTRO id=option(ID)
    { Intro id }
    | ap=STRTACTIC id=ID
    { match ap with
        | "applyt" -> Applyt id
        | "applyv" -> Applyv id 
        | _ -> failwith "absurd"
    }
    | APPLY f=form id=option(preceded(AS, ID))
    { Apply (f, id)}
    | APPLY id=ID
    { ApplyThm id }
    | QED { Qed }
    | t=SMPLTACTIC
    { match t with
        | "undia" -> Poss
        | "unbox" -> Valid
        | "fromtrue" -> FromTrue
        | "unfocus" -> Unfocus
        | _ -> failwith "absurd"
    }

form:
    LBRACE form_raw RBRACE 
    { $2 }

form_raw:
    | ID
    { Var $1 }
    | f1=form_raw ARROW f2=form_raw
    { Imp (f1, f2)}
    | BOX f=form_raw
    { Box f}
    | DIA f=form_raw
    { Dia f}
    | LPAR f=form_raw RPAR
    { f }

 env:
    | LBRACK env_list RBRACK
    { $2 }

env_list:
    | f=form 
    { [f] }
    | f=form SEMICOLON tl=env_list
    { f :: tl }

Proof K = {box p -> p} where [p] true and [x] valid.
