
/* parser.mly */

%{

open Printf
open Ast

%}

/* File parser.mly */
%token <int> NUM
%token <string> STR ID
%token INT TYPE VOID
%token IF WHILE DO FOR ELSE RETURN NEW
%token SPRINT IPRINT SCAN
%token EQ NEQ GT LT GE LE
%token PLUS MINUS TIMES DIV MOD POW INC DEC ASSIGN
%token LB RB LS RS LP RP SEMI COMMA DOTDOT
%token PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIV_ASSIGN MOD_ASSIGN
%type <Ast.stmt> prog

%right PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIV_ASSIGN MOD_ASSIGN
%nonassoc GT LT EQ NEQ GE LE
%left PLUS MINUS
%left TIMES DIV MOD
%right POW
%nonassoc UMINUS
%nonassoc INC DEC

%start prog             /* the entry point */

%%

prog : stmt  { $1 }
     ;

ty   : INT           { IntTyp }
     | INT LS NUM RS { ArrayTyp($3, IntTyp) }
     | ID	         { NameTyp $1 }
     ;

decs : decs dec { $1@$2 }
     |          { [] }
     ;

dec  : ty ids SEMI                   { List.map (fun x -> VarDec($1, x)) $2 }
     | ty ID ASSIGN expr SEMI        { [VarAssignDec($1, $2, $4)] } /* 2 */
     | TYPE ID ASSIGN ty SEMI        { [TypeDec($2, $4)] }
     | ty ID LP fargs_opt RP block   { [FuncDec($2, $4, $1, $6)] }
     | VOID ID LP fargs_opt RP block { [FuncDec($2, $4, VoidTyp, $6)] }
     ; 

ids  : ids COMMA ID { $1@[$3] }
     | ID           { [$1]  }
     ;

fargs_opt :       { [] } /* empty */
          | fargs { $1 }
          ;
     
fargs : fargs COMMA ty ID { $1@[($3,$4)] }
      | ty ID             { [($1,$2)] }
      ;

stmts : stmts stmt { $1@[$2] }
      | stmt       { [$1] }
      ;

stmt : /* ID ASSIGN expr SEMI            { Assign(Var $1, $3) } */
     /* | ID LS expr RS ASSIGN expr SEMI { Assign(IndexedVar(Var $1, $3), $6) } */
     | var ASSIGN expr SEMI           { Assign($1, $3) }
     | IF LP cond RP stmt             { If($3, $5, None) }
     | IF LP cond RP stmt ELSE stmt   { If($3, $5, Some $7) }
     | WHILE LP cond RP stmt          { While($3, $5) }
     | DO stmt WHILE LP cond RP       { DoStmt($5, $2) } /* 6 */
     | FOR LP var ASSIGN expr DOTDOT expr RP stmt
                                      { ForStmt($3, $5, $7, $9) } /* 7 */
     | SPRINT LP STR RP SEMI          { CallProc("sprint", [StrExp $3]) }
     | IPRINT LP expr RP SEMI         { CallProc("iprint", [$3]) }
     | SCAN LP ID RP SEMI             { CallProc("scan", [VarExp(Var $3)]) }
     | NEW LP ID RP SEMI              { CallProc("new", [VarExp(Var $3)]) }
     | ID LP aargs_opt RP SEMI        { CallProc($1, $3) }
     | RETURN expr SEMI               { CallProc("return", [$2]) }
     | expr2 SEMI                     { ExprStmt $1 } /* 4 */
     | block                          { $1 }
     | SEMI                           { NilStmt }
     ;

var : ID            { Var $1 }
    | ID LS expr RS { IndexedVar(Var $1, $3) }
    ;

aargs_opt :       { [] } /* empty */
          | aargs { $1 }
          ;

aargs : aargs COMMA expr { $1@[$3] }
      | expr             { [$1] }
      ;

block : LB decs stmts RB { Block($2, $3) }
      ;

expr : NUM                     { IntExp $1  }
     | ID                      { VarExp(Var $1) }
     | ID LP aargs_opt RP      { CallFunc($1, $3) } 
     | ID LS expr RS           { VarExp(IndexedVar(Var $1, $3)) }
     | expr PLUS expr          { CallFunc("+", [$1; $3]) }
     | expr MINUS expr         { CallFunc("-", [$1; $3]) }
     | expr TIMES expr         { CallFunc("*", [$1; $3]) }
     | expr DIV expr           { CallFunc("/", [$1; $3]) }
     | expr MOD expr           { CallFunc("%", [$1; $3]) } /* 1 */
     | expr POW expr           { CallFunc("^", [$1; $3]) } /* 3 */
     | MINUS expr %prec UMINUS { CallFunc("!", [$2]) }
     | LP expr RP              { $2 }
     | expr2                   { $1 }
     ;

expr2 : expr INC                { CallFunc("++", [$1]) } /* 4 */
      | expr DEC                { CallFunc("--", [$1]) } /* 4 */
      | expr PLUS_ASSIGN expr   { CallFunc("+=", [$1; $3]) } /* 5 */
      | expr MINUS_ASSIGN expr  { CallFunc("-=", [$1; $3]) } /* 5 */
      | expr TIMES_ASSIGN expr  { CallFunc("*=", [$1; $3]) } /* 5 */
      | expr DIV_ASSIGN expr    { CallFunc("/=", [$1; $3]) } /* 5 */
      | expr MOD_ASSIGN expr    { CallFunc("%=", [$1; $3]) } /* 5 */
      ;

cond : expr EQ expr  { CallFunc("==", [$1; $3]) }
     | expr NEQ expr { CallFunc("!=", [$1; $3]) }
     | expr GT expr  { CallFunc(">", [$1; $3]) }
     | expr LT expr  { CallFunc("<", [$1; $3]) }
     | expr GE expr  { CallFunc(">=", [$1; $3]) }
     | expr LE expr  { CallFunc("<=", [$1; $3]) }
     ;
%%

