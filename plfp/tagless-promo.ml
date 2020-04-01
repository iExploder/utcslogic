(* (G)ADT example *)
type expr =
| IntLit of int
| Add of expr * expr
let rec eval e = 
match e with
| IntLit(i) -> i
| Add(e1, e2) -> (eval e1) + (eval e2)
let rec display e =
match e with
| IntLit(i) -> string_of_int i
| Add(e1, e2) -> Printf.sprintf "(%s + %s)" 
				   (display e1) (display e2)
let e = Add(IntLit 1, IntLit 2)
let result = eval e 
(* -> int : 3 *)
let str_expr = display e 
(* string: "(1 + 2)" *)

(* Tagless-final example *)
module type TFExample = sig
    type expr
    val intlit : int -> expr
    val add : expr -> expr -> expr
end
module TFEval = struct
    type expr = int
    let intlit = fun x -> x
    let add = fun e1 e2 -> e1 + e2
end
module TFDisplay = struct
    type expr = string
    let intlit = fun x -> string_of_int x
    let add = fun e1 e2 -> Printf.sprintf "(%s + %s)" e1 e2
end
module Expr(T) = struct
    open T
    let s = add (intlit 1) (intlit 2)
end
let result = let module Eval = Expr(TFEval) in Eval.s
let str_expr = let module Display = Expr(TFDisplay) in Display.s