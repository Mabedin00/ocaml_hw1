type bool_expr =
  | Lit of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

let rec truth_table_h lit0 lit1 lit0_v lit1_v = function
  | Lit b -> if b = lit0 
      then lit0_v
      else lit1_v
  | Not b-> not(truth_table_h lit0 lit1 lit0_v lit1_v b) 
  | And (b, c)-> (truth_table_h lit0 lit1 lit0_v lit1_v b) && 
                 (truth_table_h lit0 lit1 lit0_v lit1_v c)
  | Or  (b, c)-> (truth_table_h lit0 lit1 lit0_v lit1_v b) ||
                 (truth_table_h lit0 lit1 lit0_v lit1_v c);; 
let truth_table lit0 lit1 b =
  [(true, true, truth_table_h lit0 lit1 true true b);
   (true, false, truth_table_h lit0 lit1 true false b);
   (false, true, truth_table_h lit0 lit1 false true b);
   (false, false, truth_table_h lit0 lit1 false false b)];; 

truth_table "a" "b" (And(Lit("a"), Lit("b")));;