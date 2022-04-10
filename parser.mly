%{
    open Ast
    open Syntax

    let locate loc x = {loc = mkLocation loc; v=x}
%}

%type <Ast.statement>statement

%token REL_KW
%token <string>ID
%token <Ast.rel_properties>PROPERITY
%token DOT
%token COLON
%token COMMA
%token THEOREM
%token PROOF
%token FALSE
%token ARROW
%token OR
%token AND
%token BOX
%token DIAMOND
%token LPAR
%token RPAR

%start statement

%%

statement:
    | rel_declaration DOT 
    { locate $loc $1 }
    | proof_statement DOT
    { locate $loc $1 }

rel_declaration:
    | REL_KW id=ID 
    { Rel (id, []) }
    | REL_KW id=ID COLON list=properity_list
    { Rel (id, list) }

properity_list:
    | p=PROPERITY option(COMMA)
    { [p] }
    | p=PROPERITY COMMA list=properity_list
    { p :: list }

proof_statement:
    | THEOREM id=ID COMMA rel_id=ID COMMA jgmt=judgement
    { Theorem (id, rel_id, jgmt) }
    | PROOF
    { Proof }
    (* Tutaj dalej będą kolejne rzeczy, 
    jak będę miał gotowy proces budowania dowodu w tył *)

judgement:
    | world=ID COLON prop=imp_prop
    { J(world, prop) }

imp_prop:
    | alt_prop ARROW imp_prop
    { Imp ($1, $3) }
    | alt_prop { $1 }

alt_prop:
    | con_prop OR alt_prop
    { Alt ($1, $3) }
    | con_prop { $1 }

con_prop:
    | atom_prop AND con_prop
    { Con ($1, $3) }
    | atom_prop { $1 }

atom_prop:
    | FALSE { F }
    | ID { Var $1 }
    | BOX atom_prop
    { Box $2 }
    | DIAMOND atom_prop
    { Dia $2 }
    | LPAR imp_prop RPAR
    { $2 }


