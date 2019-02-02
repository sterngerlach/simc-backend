
(* lexer.mll *)

{
    open Parser  
    exception No_such_symbol
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule lexer = parse
    | digit+ as num           { NUM (int_of_string num) }
    | "if"                    { IF }
    | "else"                  { ELSE }
    | "while"                 { WHILE }
    | "do"                    { DO } (* 6 *)
    | "for"                   { FOR } (* 7 *)
    | "scan"                  { SCAN }
    | "sprint"                { SPRINT }
    | "iprint"                { IPRINT }
    | "int"                   { INT }
    | "return"                { RETURN }
    | "type"                  { TYPE }
    | "void"                  { VOID }
    | id as text              { ID text }
    | '\"'[^'\"']*'\"' as str { STR str }
    | '='                     { ASSIGN }
    | "=="                    { EQ }
    | "!="                    { NEQ }
    | '>'                     { GT }
    | '<'                     { LT }
    | ">="                    { GE }
    | "<="                    { LE }
    | "++"                    { INC } (* 4 *)
    | "--"                    { DEC } (* 4 *)
    | "+="                    { PLUS_ASSIGN } (* 5 *)
    | "-="                    { MINUS_ASSIGN } (* 5 *)
    | "*="                    { TIMES_ASSIGN } (* 5 *)
    | "/="                    { DIV_ASSIGN } (* 5 *)
    | "%="                    { MOD_ASSIGN } (* 5 *)
    | '+'                     { PLUS }
    | '-'                     { MINUS }
    | '*'                     { TIMES }
    | '/'                     { DIV }
    | '%'                     { MOD } (* 1 *)
    | '^'                     { POW } (* 3 *)
    | '{'                     { LB }
    | '}'                     { RB }
    | '['                     { LS }
    | ']'                     { RS }
    | '('                     { LP }
    | ')'                     { RP }
    | ','                     { COMMA }
    | ';'                     { SEMI }
    | ".."                    { DOTDOT } (* 7 *)
    | [' ' '\t' '\n']         { lexer lexbuf } (* eat up whitespace *) 
    | eof                     { raise End_of_file }
    | _                       { raise No_such_symbol }

