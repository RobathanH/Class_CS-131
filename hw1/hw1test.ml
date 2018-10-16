(* Test subset *)
let my_subset_test0 = (subset [1;2;3;] [1;2;3;4;5;4;2;3])

(* Test equal_sets *)
let my_equal_sets_test0 = not (equal_sets [1;2;3] [3;1;3;4;2])

(* Test set_union *)
let my_set_union_test0 = 
equal_sets (set_union [1;2] [1;4;5]) [1;2;4;5]

(* Test set_intersection *)
let my_set_intersection_test0 =
equal_sets (set_intersection [1;2;3] [3;4;1;5;6]) [1;3]

(* Test set_diff *)
let my_set_diff_test0 =
equal_sets (set_diff [1;2;3;4;4;5] [4;3]) [1;2;5]

(* Test computed_fixed_point *)
let my_computed_fixed_point_test0 =
(computed_fixed_point (=) (fun x -> x*x - 2*x) 1) = 3

(* Test filter_reachable *)
let awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let startState = Expr
let rules = [(Expr, [T "A"; N Expr; T "B"]); (Expr, [N Num]); (Num, [N Expr; T "!"]); (Num, [N Binop]); (Binop, [T "*"]); (Lvalue, [N Num])]
let my_filter_reachable_test0 = (filter_reachable (startState, rules)) = (startState, [(Expr, [T "A"; N Expr; T "B"]); (Expr, [N Num]); (Num, [N Expr; T "!"]); (Num, [N Binop]); (Binop, [T "*"])])
