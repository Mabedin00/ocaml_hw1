
type 
  args = {arg1 : expr; arg2 : expr}
and
  expr = 
  | Const of int
  | Var of string
  | Plus of args
  | Mult of args
  | Minus of args
  | Div of args 
;; 

let rec evaluate = function
  | Const x -> x
  | Var x -> 1
  | Plus exp -> (evaluate exp.arg1) + (evaluate exp.arg2)
  | Minus exp -> (evaluate exp.arg1) - (evaluate exp.arg2)
  | Mult exp -> (evaluate exp.arg1) * (evaluate exp.arg2)
  | Div exp -> (evaluate exp.arg1) / (evaluate exp.arg2);;

let expr = Plus {arg1 = (Mult {arg1 = Const 2; arg2 = Const 3});
                 arg2 = (Mult {arg1 = Const 3;
                               arg2 = Minus {arg1 = Const 4; arg2 = Const 1}})}
in evaluate(expr);;