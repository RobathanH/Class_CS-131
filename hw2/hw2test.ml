(* CONVERT_GRAMMAR TEST *)
type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let rules = [(Expr, [N Num]); (Expr, [T "A"; N Expr; T "B"]); (Num, [T "!"]); (Num, [N Binop]); (Binop, [T "*"]); (Lvalue, [N Num])]
let startSym = Expr
let gram = (startSym, rules)

let converted_gram = convert_grammar gram
let expected_gram = (startSym, 
	fun x -> 
		match x with
		| Expr -> [[N Num]; [T"A"; N Expr; T"B"]]
		| Num -> [[T"!"]; [N Binop]]
		| Binop -> [[T "*"]]
	)
let my_convert_grammar_test0 = 
	let (start1, rule1) = converted_gram in
	let (start2, rule2) = expected_gram in
	(start1 = start2) && (rule1 Expr = rule2 Expr) && (rule1 Num = rule2 Num) && (rule1 Binop = rule2 Binop)



(* PARSE_PREFIX TEST *)
let accept_all = fun derive frag -> Some(derive, frag)
let testFragment = ["A"; "A"; "A"; "!"; "B"; "B"; "B"; "Rest"]
let expected_result = Some(
	[(Expr, [T "A"; N Expr; T "B"]); (Expr, [T "A"; N Expr; T "B"]);
     (Expr, [T "A"; N Expr; T "B"]); (Expr, [N Num]); (Num, [T "!"])],
	["Rest"]
	)

let my_parse_prefix_test0 = (parse_prefix converted_gram accept_all testFragment) = expected_result
