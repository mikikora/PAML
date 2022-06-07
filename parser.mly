%{
    open Relation
    open Ast
    open Syntax

    let locate = Lexer.locate
%}

%token RELATION
%token <string>ID
%token DOT
%token UNSET // unset property for relation 
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
%token ASS
%token TRY
%token AUTO
%token EXIT
%token MODEL
%token HINT
%token <bool>ONOFF
%token SHOW
%token EQUAL
%token WHERE

%token SEMICOLON
%token LBRACE
%token RBRACE
%token ASSUMPTION
%token SINGLE
%token DOUBLE
%token TRIPLE
%token <Syntax.theorem_rule>RULE
%token DIAE
%token RD
%token RT
%token TWO
%token VDASH
%token INTROCMD
%token APPLYCMD
%token APPLYASSMCMD
%token APPLYTHCMD
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
%token CHAINCMD
%token TRYCMD
%token AUTOCMD
%token NONE
%token SOME
%token EMPTY_ASSMP
%token LOAD
%token <string>FILE_NAME
%token SAVE
%token LATEX
%token EQUIV
%token NEG
%token TRUE

%type <Ast.statement>statement
%type <Syntax.prop>alt_prop
%type <Syntax.prop>atom_prop
%type <Syntax.prop>con_prop
%type <Syntax.prop>imp_prop
%type <Syntax.prop>equiv_prop
%type <Ast.command>command
%type <Syntax.judgement>judgement
%type <string option>option(ID)
%type <Ast.statement_raw>statement_raw
%type <Relation.rel_property list>property_list
%type <Relation.rel_property list>not_empty_property_list
%type <Relation.rel_property>relation_property
%type <unit option>option(COMMA)
%type <string option>option(preceded(AS, ID))
%type <string option>option(preceded(COMMA, ID))
%type <string option>option(preceded(WITH, ID))
%type <(string * Syntax.prop) list>assingments_list
%type <string * Syntax.prop>assingment

%type <Relation.relation list * 
        (string * Syntax.theorem) list * 
        (string * string * Syntax.judgement * Ast.command list) option>backup
%type <Relation.relation list>relation_backup_list
%type <(string * Syntax.theorem) list>theorem_backup_list
%type <(string * string * Syntax.judgement * Ast.command list) option>proof_backup
%type <Ast.command list>proof_commands_backup_list
%type <Relation.relation>relation_backup
%type <string * Syntax.theorem>theorem_backup
%type <Syntax.theorem>theorem
%type <Syntax.theorem_context>theorem_context
%type <Ast.command>proof_command_backup
%type <string option>option_string
%type <Syntax.judgement list>assumptions
%type <Syntax.judgement>extended_judgement
%type <Syntax.theorem_rule>rule

%type <bool>answer

%left SEMICOLON
%right TRY

%start statement
%start backup
%start answer

%%

statement:
    | statement_raw DOT 
    { locate $1 }

statement_raw:
    | RELATION ID property_list
    { RelDecl ($2, $3) }
    | ID not_empty_property_list
    { RelProperties ($1, $2) }
    | ID UNSET not_empty_property_list
    { RelRmProperties ($1, $3) }
    | THEOREM id=ID rel=option(preceded(WITH, ID)) COMMA jgmt=judgement
    { TheoremDecl (id, rel, jgmt) }
    | THEOREM id=ID rel=option(preceded(WITH, ID)) COMMA prop=equiv_prop
    { TheoremDecl (id, rel, J("x", prop)) }
    | LOAD name=FILE_NAME
    { LoadBackup name }
    | SAVE name=FILE_NAME
    { SaveBackup name }
    | LATEX name=FILE_NAME
    { GenerateLatex name }
    | EXIT { raise Lexer.Eof }
    | MODEL system=ID rel_name=option(preceded(WITH, ID))
    { 
        let sys = (match system with
          | "K" -> K
          | "T" -> T
          | "B" -> B
          | "S4" -> S4
          | "S5" -> S5
          | _ -> (* message ignored*)
            raise (Error.UnlocatedError "Not a proper system name") 
        ) in
        EnterModel (rel_name, sys) }
    | EXIT MODEL
    { ExitModel }
    | HINT b=ONOFF
    { ToggleHints b }
    | SHOW { ShowCmd }
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
    | ASS {AssumptionCmd}

    | CONTRA WITH world=ID
    { ContraCmd world }
    | FOCUS n=NUM
    { FocusCmd n }
    | FOCUS
    { FocusCmd 1 }
    | APPLY asm=ID world=option(preceded(WITH, ID))
    { ApplyAssmCmd (None, None, world, asm) }
    | APPLY asm=ID world=option(preceded(WITH, ID)) AS name1=ID name2=option(preceded(COMMA, ID))
    { ApplyAssmCmd (Some name1, name2, world, asm) }
    | INTRO name=option(ID) world=option(preceded(WITH, ID))
    { IntroCmd (name, world) }
    | APPLY jgmt=judgement world=option(preceded(WITH, ID))
    { ApplyCmd (None, None, world, jgmt)}
    | APPLY jgmt=judgement world=option(preceded(WITH, ID)) AS name1=ID name2=option(preceded(COMMA, ID))
    { ApplyCmd (Some name1, name2, world, jgmt) }
    | APPLY th=ID world=option(preceded(WITH, ID)) WHERE assingments=assingments_list
    { ApplyThCmd (None, None, world, th, assingments) }
    | APPLY th=ID world=option(preceded(WITH, ID)) AS name1=ID name2=option(preceded(COMMA, ID)) WHERE assingments=assingments_list
    { ApplyThCmd (Some name1, name2, world, th, assingments)}
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
    | DIRECT 
      WITH world1=ID COMMA world2=ID COMMA world3=ID world4=option(preceded(COMMA, ID)) 
      AS name1=ID COMMA name2=ID
    { DirectCmd (Some name1, Some name2, world1, world2, world3, world4) }

    | cmd1=command SEMICOLON cmd2=command
    { ChainCmd (cmd1, cmd2) }
    | TRY cmd=command
    { TryCmd cmd }
    | AUTO n=NUM
    { AutoCmd n }
    | AUTO
    { AutoCmd 5 }

assingments_list:
    | assingment=assingment
    { [assingment] }
    | assingment=assingment COMMA tl=assingments_list
    { assingment :: tl }

assingment:
    variable=ID COLON EQUAL prop=equiv_prop
    { variable, prop }

judgement:
    | world=ID COLON prop=equiv_prop
    { J (world, prop) }

equiv_prop:
    | imp_prop EQUIV equiv_prop
    { Con (Imp ($1, $3), Imp($3, $1)) }
    | imp_prop { $1 }

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
    | NEG atom_prop
    { Imp ($2, F) }
    | TRUE { Imp (F, F) }
    | LPAR equiv_prop RPAR
    { $2 }


backup:
    r=relation_backup_list SEMICOLON SEMICOLON 
    t=theorem_backup_list SEMICOLON SEMICOLON 
    p=proof_backup SEMICOLON SEMICOLON
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
    | name=ID r=ID jgmt=extended_judgement cmd_lst=proof_commands_backup_list
    { Some (name, r, jgmt, List.rev cmd_lst) }

proof_commands_backup_list:
    //empty
    { [] }
    | proof_command_backup proof_commands_backup_list
    { $1::$2 }


relation_backup:
    | LBRACE name=ID properties=property_list RBRACE
    { {name; properties} }

theorem_backup:
    | LBRACE name=ID th=theorem RBRACE
    { (name, th) }

theorem:
    | ASSUMPTION rule=rule thx=theorem_context
    { Assumption (rule, thx) }
    | SINGLE rule=rule th1=theorem thx=theorem_context
    { Single (rule, th1, thx) }
    | DOUBLE rule=rule th1=theorem th2=theorem thx=theorem_context
    { Double (rule, th1, th2, thx) }
    | TRIPLE rule=rule th1=theorem th2=theorem th3=theorem thx=theorem_context
    { Triple (rule, th1, th2, th3, thx) }

rule:
    | RULE {$1}
    | DIAE x=ID {DiaE x}
    | RD x=ID y=ID {D (x, y)}
    | RT x=ID {T x}
    | TWO x=ID {Two x}

theorem_context:
    | r=ID COLON COLON ass=assumptions VDASH jgmt=extended_judgement
    { (r, ass, jgmt) }

assumptions:
    //empty
    { [] }
    | EMPTY_ASSMP
    { [] }
    | jgmt=extended_judgement SEMICOLON tl=assumptions
    {jgmt::tl}

proof_command_backup:
    | INTROCMD name=option_string COMMA world=option_string
    { IntroCmd (name, world) }
    | APPLYCMD name1=option_string COMMA 
      name2=option_string COMMA 
      world=option_string COMMA 
      jgmt=judgement
    { ApplyCmd (name1, name2, world, jgmt) }
    | APPLYASSMCMD name1=option_string COMMA 
      name2=option_string COMMA 
      world=option_string COMMA 
      name=ID
    { ApplyAssmCmd (name1, name2, world, name) }
    | APPLYTHCMD name1=option_string COMMA
      name2=option_string COMMA
      world=option_string COMMA
      name=ID COMMA
      assingments=assingments_list
      { ApplyThCmd (name1, name2, world, name, assingments) }
    | SPLITCMD { SplitCmd }
    | LEFTCMD { LeftCmd }
    | RIGHTCMD {RightCmd}
    | SERIALCMD name=option_string COMMA world1=option_string COMMA world2=ID
    { SerialCmd (name, world1, world2) }
    | REFLCMD name=option_string COMMA world=ID
    { ReflCmd (name, world) }
    | SYMMCMD name=option_string COMMA world1=ID COMMA world2=ID
    { SymmCmd (name, world1, world2) }
    | TRANSCMD name=option_string COMMA world1=ID COMMA world2=ID COMMA world3=ID
    { TransCmd (name, world1, world2, world3) }
    | EUCLCMD name=option_string COMMA world1=ID COMMA world2=ID COMMA world3=ID
    { EuclCmd (name, world1, world2, world3) }
    | DIRECTCMD name1=option_string COMMA 
      name2=option_string COMMA 
      world1=ID COMMA 
      world2=ID COMMA 
      world3=ID COMMA 
      world4=option_string
    { DirectCmd (name1, name2, world1, world2, world3, world4) }
    | PROOFCMD {ProofCmd}
    | FOCUSCMD n=NUM {FocusCmd n}
    | UNFOCUSCMD {UnfocusCmd}
    | CONTRACMD world=ID {ContraCmd world}
    | ASSUMPTIONCMD {AssumptionCmd}
    | CHAINCMD cmd1=proof_command_backup cmd2=proof_command_backup
    { ChainCmd (cmd1, cmd2) }
    | TRYCMD cmd=proof_command_backup
    { TryCmd cmd }
    | AUTOCMD n=NUM
    { AutoCmd n }

option_string:
    | NONE
    { None }
    | SOME a=ID
    { Some a }

extended_judgement:
    | world1=ID ID world2=ID
    { R(world1, world2) }
    | judgement
    { $1 }


answer:
    | id=ID
    {
        if id = "yes" || id = "y" then true
        else if id = "no" || id = "n" then false
        else raise (Lexer.InvalidToken (locate id, "Not an answer"))
    }