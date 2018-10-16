open List

(* Helper function for convert_grammar *)

let rec join_rules rules inSym = match rules with
	| [] -> []
	| (lhs, rhs)::rest ->
		if inSym = lhs then
			rhs::(join_rules rest inSym)
		else
			join_rules rest inSym

let convert_grammar gram1 =
	let (startSym, rules) = gram1 in
	(startSym, (fun x -> join_rules rules x))



(* Symbol type declaration *)
type ('a, 'b) symbol = 
	| N of 'a
	| T of 'b

(* returns the first nonterminal symbol in state (list of symbols) in the format (symbolsBefore, nonterminal, symbolsAfter) *)
(* state MUST include a nonterminal symbol or it won't work *)
let rec firstNT state =
	let current::rest = state in
	match current with
		| N _ -> ([], current, rest)
		| _ ->
			let (first, mid, last) = firstNT (tl state) in
			(current::first, mid, last)

(* returns true if the state contains a nonterminal symbol *)
let rec containsNT state = match state with
	| [] -> false
	| (N _)::_ -> true
	| _ -> containsNT (tl state)

(* given replacement (the rhs of a derivation rule), this will replace the first nonterminal symbol in state with replacement *)
let applyRule state replacement =
	let (first, mid, last) = firstNT state in
	first @ replacement @ last

(* Recursive function in charge of dispatching new instances of the derive function *)
let rec createNewDerives curDerivTree curState acceptor rules nonT newDerivList frag = match newDerivList with
	| [] -> None
	| newDeriv::restOfDerivs ->
		let newDerivTree = curDerivTree @ [(nonT, newDeriv)] in
		let newState = applyRule curState newDeriv in
		let result = derive newDerivTree newState acceptor rules frag in
		if result = None then
			createNewDerives curDerivTree curState acceptor rules nonT restOfDerivs frag
		else
			result

and derive curDerivTree curState acceptor rules frag =
	if (containsNT curState) then
		let (first, N nonT, _) = firstNT curState in
		match frag with
			| first::rest ->
				let possibleDerivs = rules nonT in
				createNewDerives curDerivTree curState acceptor rules nonT possibleDerivs frag
			| _ -> None
	else
		match frag with
			| curState::suffix ->
				acceptor curDerivTree suffix
			| _ -> None

let parse_prefix gram = match gram with (startNonT, rules) ->
	(fun accept frag -> derive [] [N startNonT] accept rules frag)


(* TESTS *)
type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let rules = [(Expr, [T "A"; N Expr; T "B"]); (Expr, [N Num]); (Num, [N Expr; T "!"]); (Num, [N Binop]); (Binop, [T "*"]); (Lvalue, [N Num])]
let startSym = Expr
let gram = (startSym, rules)

