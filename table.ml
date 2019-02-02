
(* table.ml *)

open Types

type varInfo = { offset: int; level: int; ty: ty }
type funInfo = { formals: ty list; result: ty; level: int }
type enventry = VarEntry of varInfo | FunEntry of funInfo

exception No_such_symbol of string
exception SymErr of string
let initTable = fun x -> raise (No_such_symbol x)
let update var vl t = function x -> if x = var then vl else t x

