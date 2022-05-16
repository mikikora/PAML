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
%token ASSUMPTION

%token SEMICOLON
%token LBRACE
%token RBRACE
%token FALSEE
%token HYP
%token CONI
%token CONE
%token ALTI
%token ALTE
%token IMPI
%token IMPE
%token BOXI
%token BOXE
%token DIAI
%token DIAE
%token RD
%token RT
%token RB
%token FOUR
%token FIVE
%token TWO
%token VDASH
%token INTROCMD
%token APPLYCMD
%token APPLYASSMCMD
%token SPLITCMD
%token LEFTCMD
%token RIGHTCMD
%token SERIALCMD
%token REFLCMD
%token SYMMCMD
%token TRANSCMD
%token EUCLCMD
%token DIRECTCMD
%token PROOFCMD
%token FOCUSCMD
%token UNFOCUSCMD
%token CONTRACMD
%token ASSUMPTIONCMD
%token NONE
%token SOME

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
%type <unit option>option(COMMA)
%type <string option>option(preceded(AS, ID))
%type <string option>option(preceded(COMMA, ID))
%type <string option>option(preceded(WITH, ID))

%type <Relation.relation list * (string * string * theorem) list * (string * judgement * command list) option>backup
%type <Relation.relation list>relation_backup_list
%type <(string * string * theorem) list>theorem_backup_list
%type <(string * judgement * command list) option>proof_backup
%type <command list>proof_commands_backup_list
%type <relation>relation_backup
%type <string * string * theorem>theorem_backup
%type <theorem>theorem
%type <theorem_context>theorem_context
%type <command>proof_command_backup
%type <string option>option_string



%start statement
%start backup

%%

statement:
    statement_raw DOT 
    { locate $1 }

statement_raw:
    | RELATION ID property_list
    { RelDecl ($2, $3) }
    | ID not_empty_property_list
    { RelProperties ($1, $2) }
    | ID UNSET not_empty_property_list
    { RelRmProperties ($1, $3) }
    | THEOREM id=ID WITH rel=ID COMMA jgmt=judgement
    { TheoremDecl (id, rel, jgmt) }
    | THEOREM id=ID WITH rel=ID COMMA prop=imp_prop
    { TheoremDecl (id, rel, J("x", prop)) }
    | command
    { Command $1 }

property_list:
    // empty
    { [] }
    | not_empty_property_list
    { $1 }
    
not_empty_property_list:
    | relation_property option(COMMA) property_list
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
    | ASSUMPTION {AssumptionCmd}

    | CONTRA WITH world=ID
    { ContraCmd world }
    | FOCUS n=NUM
    { FocusCmd n }
    | FOCUS
    { FocusCmd 1 }
    | APPLY asm=ID
    { ApplyAssmCmd asm }
    | INTRO name=option(ID) world=option(preceded(WITH, ID))
    { IntroCmd (name, world) }
    | APPLY jgmt=judgement world=option(preceded(WITH, ID))
    { ApplyCmd (None, None, world, jgmt)}
    | APPLY jgmt=judgement world=option(preceded(WITH, ID)) AS name1=ID COMMA name2=ID
    { ApplyCmd (Some name1, Some name2, world, jgmt) }
    | SERIAL WITH world2=ID world1=option(preceded(COMMA, ID)) name=option(preceded(AS, ID))
    { SerialCmd (name, world1, world2) }
    | REFL WITH world=ID name=option(preceded(AS, ID))
    { ReflCmd (name, world) }
    | SYMM WITH world1=ID COMMA world2=ID name=option(preceded(AS, ID))
    { SymmCmd (name, world1, world2) }
    | TRANS WITH world1=ID COMMA world2=ID COMMA world3=ID name=option(preceded(AS, ID))
    { TransCmd (name, world1, world2, world3) }
    | EUCL WITH world1=ID COMMA world2=ID COMMA world3=ID name=option(preceded(AS, ID))
    { EuclCmd (name, world1, world2, world3) }
    | DIRECT WITH world1=ID COMMA world2=ID COMMA world3=ID world4=option(preceded(COMMA, ID))
    { DirectCmd (None, None, world1, world2, world3, world4) }
    | DIRECT WITH world1=ID COMMA world2=ID COMMA world3=ID world4=option(preceded(COMMA, ID)) AS name1=ID COMMA name2=ID
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


backup:
    r=relation_backup_list SEMICOLON SEMICOLON t=theorem_backup_list SEMICOLON SEMICOLON p=proof_backup
    {r, t, p}

relation_backup_list:
    //empty
    { [] }
    | relation_backup relation_backup_list
    { $1::$2 }

theorem_backup_list:
    //empty
    { [] }
    | theorem_backup theorem_backup_list
    { $1::$2 }

proof_backup:
    //empty
    { None }
    | name=ID jgmt=judgement cmd_lst=proof_commands_backup_list
    { Some (name, jgmt, cmd_lst) }

proof_commands_backup_list:
    | proof_command_backup
    { [$1] }
    | proof_command_backup proof_commands_backup_list
    { $1::$2 }


relation_backup:
    | LBRACE name=ID properties=property_list RBRACE
    { {name; properties} }

theorem_backup:
    | LBRACE name=ID r=ID th=theorem RBRACE
    { (name, r, th) }

theorem:
    | FALSEE th=theorem thx=theorem_context
    { FalseE (th, thx) }
    | HYP thx=theorem_context
    { Hyp thx }
    | CONI th1=theorem th2=theorem = thx=theorem_context
    { ConI (th1, th2, thx)}
    | CONE th=theorem thx=theorem_context
    { ConE (th, thx) }
    | ALTI th=theorem thx=theorem_context
    { AltI (th, thx) }
    | ALTE th1=theorem th2=theorem th3=theorem thx=theorem_context
    { AltE (th1, th2, th3, thx) }
    | IMPI th=theorem thx=theorem_context
    { ImpI (th, thx) }
    | IMPE th1=theorem th2=theorem = thx=theorem_context
    { ImpE (th1, th2, thx)}
    | BOXI th=theorem thx=theorem_context
    { BoxI (th, thx) }
    | BOXE th1=theorem th2=theorem = thx=theorem_context
    { BoxE (th1, th2, thx)}
    | DIAI th1=theorem th2=theorem = thx=theorem_context
    { DiaI (th1, th2, thx)}
    | DIAE th1=theorem th2=theorem = thx=theorem_context
    { DiaE (th1, th2, thx)}
    | RD th=theorem thx=theorem_context
    { D (th, thx) }
    | RT th=theorem thx=theorem_context
    { T (th, thx) }
    | RB th=theorem thx=theorem_context
    { B (th, thx) }
    | FOUR th1=theorem th2=theorem th3=theorem thx=theorem_context
    { Four (th1, th2, th3, thx) }
    | FIVE th1=theorem th2=theorem th3=theorem thx=theorem_context
    { Five (th1, th2, th3, thx) }
    | TWO th1=theorem th2=theorem th3=theorem thx=theorem_context
    { Two (th1, th2, th3, thx) }


theorem_context:
    | r=ID COLON COLON ass=assumptions VDASH jgmt=judgement
    { (r, ass, jgmt) }

assumptions:
    //empty
    { [] }
    | EMPTY_ASSMP
    { [] }
    | jgmt=judgement SEMICOLON assumptions

proof_command_backup:
    | INTROCMD name=option_string COMMA world=option_string
    { IntroCmd (name, world) }
    | APPLYCMD name1=option_string COMMA name2=option_string COMMA world=option_string COMMA jgmt=judgement
    { ApplyCmd (name1, name2, world, jgmt) }
    | APPLYASSMCMD name=ID
    { ApplyAssmCmd name }
    | SPLITCMD { SplitCmd }
    | LEFTCMD { LeftCmd }
    | RIGHTCMD {RightCmd}
    | SERIALCMD name=option_string COMMA world1=option_string COMMA world2=ID
    { SerialCmd (name, world1, world2) }
    | REFLCMD name=option_string COMMA world=ID
    { ReflCmd (name, world) }
    | SYMMCMD name=option_string COMMA world1=ID COMMA world2=ID
    { SymmCmd (name, world1, world2) }
    | TRANSCMD name=option_string COMMA world1=ID COMMA world2=ID
    { TransCmd (name, world1, world2) }
    | EUCLCMD name=option_string COMMA world1=ID COMMA world2=ID
    { EuclCmd (name, world1, world2) }
    | DIRECTCMD name1=option_string COMMA name2=option_string COMMA world1=ID COMMA world2=ID COMMA world3=ID COMMA world4=option_string
    { DirectCmd (name1, name2, world1, world2, world3, world4) }
    | PROOFCMD {ProofCmd}
    | FOCUSCMD n=NUM {FocusCmd n}
    | UNFOCUSCMD {UnfocusCmd}
    | CONTRACMD world=ID {ContraCmd world}
    | ASSUMPTIONCMD {AssumptionCmd}

option_string:
    | NONE
    { None }
    | SOME a=ID
    { Some a }