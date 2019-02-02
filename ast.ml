
(* ast.ml *)
(* The definition of the abstract syntax tree *)

type id = string
type var = Var of id | IndexedVar of var * exp
and stmt = Assign of var * exp        
         | CallProc of id * (exp list)
         | Block of (dec list) * (stmt list)
         | If of exp * stmt * (stmt option)
         | While of exp * stmt
         | DoStmt of exp * stmt
         | ForStmt of var * exp * exp * stmt
         | ExprStmt of exp
         | NilStmt
and exp = VarExp of var | StrExp of string | IntExp of int 
        | CallFunc of id * (exp list)
and dec = FuncDec of id * ((typ * id) list) * typ * stmt
        | TypeDec of id * typ
        | VarDec of typ * id
        | VarAssignDec of typ * id * exp (* 2 *)
and typ = NameTyp of string
        | ArrayTyp of int * typ
        | IntTyp
        | VoidTyp

