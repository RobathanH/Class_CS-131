% -------------------- TOWER -------------------------------

tower(N, T, C) :-
	createEmptyT(N, T),
	allRowsValid(N, T),
	allColsValid(N, T),

	C = counts(Top, Bot, Left, Right),
	fds_visibility_top(N, 1, T, Top),
	fds_visibility_bot(N, 1, T, Bot),
	fds_visibility_left(N, 1, T, Left),
	fds_visibility_right(N, 1, T, Right).


% Initializes T to a correctly shaped array (N x N)
createEmptyT(N, T) :-
	length(T, N),
	createEmptyT_helper(N, T).	

createEmptyT_helper(_, []).
createEmptyT_helper(N, [H|T]) :-
	length(H, N),
	createEmptyT_helper(N, T).
	

% Checks that all rows are permutations of [1...N]
allRowsValid(_, []).
allRowsValid(N, [H|T]) :-
	validList(N, H),
	allRowsValid(N, T).

% Checks that all columns are permutations of [1...N]
allColsValid(N, T) :-
	allColsValid_helper(N, 1, T).

allColsValid_helper(N, I, _) :-
	I #># N.
allColsValid_helper(N, I, T) :-
	I #=<# N,
	
	ithCol(N, I, T, L),
	validList(N, L),

	Inew #=# I + 1,
	allColsValid_helper(N, Inew, T).


% true if list/column L is a permutation of [1...N]
validList(_, []).
validList(N, [H|T]) :-
	H #=<# N,
	H #>=# 1,
	validList_helper(H, T),
	validList(N, T).

validList_helper(_, []).
validList_helper(El, [H|T]) :-
	El #\=# H,
	validList_helper(El, T).


% Visibility predicate using fds operators
fds_visibility(N, L, V) :-
	fds_visibilityHelper(N, 0, V, L).

fds_visibilityHelper(N, N, 0, []).
fds_visibilityHelper(N, MaxNum, V, [H|T]) :-
	H #># MaxNum,
	fds_visibilityHelper(N, H, Vnew, T), 
	V #=# Vnew + 1.
fds_visibilityHelper(N, MaxNum, V, [H|T]) :-
	H #<# MaxNum,
	fds_visibilityHelper(N, MaxNum, Vnew, T), 
	V #=# Vnew.

% CALCULATES FDS VISIBILITY COUNT LIST FOR A DIRECTION, using fds operators
% I = index, starting at 1, and goes to N
fds_visibility_top(N, I, _, []) :- I #># N.
fds_visibility_top(N, I, T, Top) :-
	I #=<# N,
	
	Inew #=# I + 1,
	fds_visibility_top(N, Inew, T, TopRest),
	
	ithCol(N, I, T, L),
	fds_visibility(N, L, TopHead),
	Top = [TopHead | TopRest].

fds_visibility_bot(N, I, _, []) :- I #># N.
fds_visibility_bot(N, I, T, Bot) :-
	I #=<# N,
	
	Inew #=# I + 1,
	fds_visibility_bot(N, Inew, T, BotRest),

	ithCol(N, I, T, Lrev),
	reverse(Lrev, L),
	fds_visibility(N, L, BotHead),
	Bot = [BotHead | BotRest].

fds_visibility_left(N, I, _, []) :- I #># N.
fds_visibility_left(N, I, T, Left) :-
	I #=<# N,
	
	Inew #=# I + 1,
	fds_visibility_left(N, Inew, T, LeftRest),

	ithRow(N, I, T, L),
	fds_visibility(N, L, LeftHead),
	Left = [LeftHead | LeftRest].

fds_visibility_right(N, I, _, []) :- I #># N.
fds_visibility_right(N, I, T, Right) :-
	I #=<# N,
	
	Inew #=# I + 1,
	fds_visibility_right(N, Inew, T, RightRest),

	ithRow(N, I, T, Lrev),
	reverse(Lrev, L),
	fds_visibility(N, L, RightHead),
	Right = [RightHead | RightRest].




% ------------------- PLAIN TOWER ---------------------------------

plain_tower(N, T, C) :-
	format_T(N, T),
	C = counts(Top, Bot, Left, Right),
	visibility_top(N, 1, T, Top),
	visibility_bot(N, 1, T, Bot),
	visibility_left(N, 1, T, Left),
	visibility_right(N, 1, T, Right).

% FORMAT T - make it an nxn matrix with each row/column a permutation of [1...N]
format_T(N, T) :-
	length(T, N),
	format_T_rows(N, 1, T),
	format_T_cols(N, 1, T).

format_T_rows(N, I, _) :- I > N.
format_T_rows(N, I, T) :-
	I =< N,
	
	ithRow(N, I, T, L),
	
	range(N, Nrange),
	permutation(Nrange, L),		

	Inew is I + 1,
	format_T_rows(N, Inew, T).

format_T_cols(N, I, _) :- I > N.
format_T_cols(N, I, T) :-
	I =< N,

	ithCol(N, I, T, L),

	range(N, Nrange),
	permutation(Nrange, L),

	Inew is I + 1,
	format_T_cols(N, Inew, T).
	

% CALCULATES VISIBILITY COUNT LIST FOR A DIRECTION
% I = index, starting at 1, and goes to N
visibility_top(N, I, _, Top) :-
	I > N,
	Top = [].
visibility_top(N, I, T, Top) :-
	I =< N,
	
	Inew is I + 1,
	visibility_top(N, Inew, T, TopRest),
	
	ithCol(N, I, T, L),
	visibility(N, L, TopHead),
	Top = [TopHead | TopRest].

visibility_bot(N, I, _, Bot) :-
	I > N,
	Bot = [].
visibility_bot(N, I, T, Bot) :-
	I =< N,
	
	Inew is I + 1,
	visibility_bot(N, Inew, T, BotRest),

	ithCol(N, I, T, Lrev),
	reverse(Lrev, L),
	visibility(N, L, BotHead),
	Bot = [BotHead | BotRest].

visibility_left(N, I, _, Left) :-
	I > N,
	Left = [].
visibility_left(N, I, T, Left) :-
	I =< N,
	
	Inew is I + 1,
	visibility_left(N, Inew, T, LeftRest),

	ithRow(N, I, T, L),
	visibility(N, L, LeftHead),
	Left = [LeftHead | LeftRest].

visibility_right(N, I, _, Right) :-
	I > N,
	Right = [].
visibility_right(N, I, T, Right) :-
	I =< N,
	
	Inew is I + 1,
	visibility_right(N, Inew, T, RightRest),

	ithRow(N, I, T, Lrev),
	reverse(Lrev, L),
	visibility(N, L, RightHead),
	Right = [RightHead | RightRest].


% CALCULATES VISIBILITY COUNT FOR A LIST

visibility(N, L, V) :-
	range(N, Nrange),
	permutation(Nrange, L),
	visibilityHelper(N, 0, V, L).

visibilityHelper(N, N, 0, []).
visibilityHelper(N, MaxNum, V, [H|T]) :-
	H > MaxNum,
	visibilityHelper(N, H, Vnew, T), 
	V is Vnew + 1.
visibilityHelper(N, MaxNum, V, [H|T]) :-
	H < MaxNum,
	visibilityHelper(N, MaxNum, Vnew, T), 
	V is Vnew.


% General helper function, L is a list from 1 to N
range(0, []) :- !.
range(N, L) :-
	N1 is N - 1,
	range(N1, L1),
	append(L1, [N], L).


% SELECTING CERTAIN COLUMNS AND ROWS OF T
% I = index (from 1 to N)
% N = N, the number of rows/columns in T
% T = the main array of tower heights
% L = the resulting list of length N
ithCol(N, I, T, L) :-
	range(N, Nrange),
	member(I, Nrange),
	ithCol_helper(N, 1, I, T, L).

ithCol_helper(N, RowNum, _, _, L) :-
	RowNum > N,
	L = [].
ithCol_helper(N, RowNum, ColNum, T, L) :-
	RowNum =< N,
	NewRowNum is RowNum + 1,
	ithCol_helper(N, NewRowNum, ColNum, T, Lrest),
	accessElement(RowNum, ColNum, T, El),
	L = [El|Lrest].
	
accessElement(RowNum, ColNum, T, El) :-	
	nth(RowNum, T, Row),
	nth(ColNum, Row, El).

ithRow(_, I, T, L) :-
	nth(I, T, L).




% ------------------ AMBIGUOUS -----------------------------
ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \== T2.


% ---------------------TEST CASE (SPEEDUP) ----------------------------
% Finds first solution to N=4 case with counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])
% tower\3 is run 16 times to avoid Time_FDS rounding to zero and causing a divide by zero exception
speedup(S) :-
	statistics(cpu_time, [Start_plain | _]),
	plain_tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	statistics(cpu_time, [End_plain | _]),
	Time_plain is End_plain - Start_plain,

	statistics(cpu_time, [Start_FDS | _]),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	tower(4, _, counts([1,2,2,4],[4,2,2,1],[1,2,2,3],[3,2,2,1])),
	statistics(cpu_time, [End_FDS | _]),
	Time_FDS is End_FDS - Start_FDS,
	
	write(Time_FDS),

	S is 16 * Time_plain / Time_FDS,
	!.
