%{
    open Relation
    open Ast
    open Syntax

    let locate = Lexer.locate
%}

%token RELATION
%token <string>ID
%token DOT
%token UNSET // Not relation property 
%token ABANDON
%token QED
%token THEOREM 
%token PROOF
%token WITH
%token AND OR IMPL BOX DIA FALSE
%token INTRO
%token APPLY
%token SPLIT
%token LEFT
%token RIGHT
%token SERIAL
%token REFL
%token SYMM
%token TRANS
%token EUCL
%token DIRECT
%token LPAR
%token RPAR
%token COMMA
%token COLON
%token AS
%token FOCUS
%token <int>NUM
%token UNFOCUS
%token CONTRA
%token UNDO

%type <Ast.statement>statement
%type <Syntax.prop>alt_prop
%type <Syntax.prop>atom_prop
%type <Syntax.prop>con_prop
%type <Syntax.prop>imp_prop
%type <Ast.command>command
%type <Syntax.judgement>judgement
%type <string option>option(ID)
%type <Ast.statement_raw>statement_raw
%type <Relation.rel_properties list>property_list
%type <Relation.rel_properties list>not_empty_property_list
%type <Relation.rel_properties>relation_property


%start statement

%%

statement:
    statement_raw DOT 
    { locate $1 }

statement_raw:
    | RELATION ID property_list
    { RelDecl ($2, $3) }
    | ID not_empty_property_list
    { RelProperties ($1, $2) }
    | ID UNSET property_list
    { RelRmProperties ($1, $3) }
    | THEOREM id=ID WITH rel=ID COMMA jgmt=judgement
    { TheoremDecl (id, rel, jgmt) }
    | command
    { Command $1 }

property_list:
    // empty
    { [] }
    | not_empty_property_list
    { $1 }
    
not_empty_property_list:
    | relation_property 
    { [$1] }
    | relation_property COMMA property_list
    { $1::$3 }

relation_property:
    | SERIAL
    { Relation.Seriality }
    | REFL
    { Relation.Reflexivity }
    | SYMM
    { Relation.Symmetry }
    | TRANS
    { Relation.Transitivity }
    | EUCL
    { Relation.Euclideanness }
    | DIRECT
    { Relation.Directedness }

command:
    | PROOF     { ProofCmd }
    | QED       { QedCmd }
    | ABANDON   { AbandonCmd }
    | RIGHT     { RightCmd }
    | LEFT      { LeftCmd }
    | SPLIT     { SplitCmd }
    | UNFOCUS   { UnfocusCmd }
    | UNDO      { UndoCmd }

    | CONTRA WITH world=ID
    { ContraCmd world }
    | FOCUS n=NUM
    { FocusCmd n }
    | FOCUS
    { FocusCmd 1 }
    | APPLY asm=ID
    { ApplyAssmCmd asm }
    | INTRO name=option(ID)
    { IntroCmd (name, None) }
    | INTRO name=option(ID) WITH world=ID
    { IntroCmd (name, Some world) }
    | APPLY jgmt=judgement world=preceded(WITH, option(ID))
    { ApplyCmd (None, None, world, jgmt)}
    | APPLY jgmt=judgement world=preceded(WITH, option(ID)) AS name1=ID COMMA name2=ID
    { ApplyCmd (Some name1, Some name2, world, jgmt) }
    | SERIAL WITH world1=terminated(option(ID), COMMA) world2=ID name=preceded(AS, option(ID))
    { SerialCmd (name, world1, world2) }
    | REFL WITH world=ID name=preceded(AS, option(ID))
    { ReflCmd (name, world) }
    | SYMM WITH world1=ID COMMA world2=ID name=preceded(AS, option(ID))
    { SymmCmd (name, world1, world2) }
    | TRANS WITH world1=ID COMMA world2=ID COMMA world3=ID name=preceded(AS, option(ID))
    { TransCmd (name, world1, world2, world3) }
    | EUCL WITH world1=ID COMMA world2=ID COMMA world3=ID name=preceded(AS, option(ID))
    { EuclCmd (name, world1, world2, world3) }
    | DIRECT WITH world1=ID COMMA world2=ID COMMA world3=ID world4=preceded(COMMA, option(ID)) 
    { DirectCmd (None, None, world1, world2, world3, world4) }
    | DIRECT WITH world1=ID COMMA world2=ID COMMA world3=ID world4=preceded(COMMA, option(ID)) AS name1=ID COMMA name2=ID
    { DirectCmd (Some name1, Some name2, world1, world2, world3, world4) }

judgement:
    | world=ID COLON prop=imp_prop
    { J (world, prop) }

imp_prop:
    | alt_prop IMPL imp_prop
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
    | DIA atom_prop
    { Dia $2 }
    | LPAR imp_prop RPAR
    { $2 }


