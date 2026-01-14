drop_every_n(List, N, Result):-
	drop_every_n(List,N,Result,N).

drop_every_n([],_,[],_).

drop_every_n([_|T],N,Result,1):-
	drop_every_n(T,N,Result,N).

drop_every_n([H|_T],N,[H|Rest],Acc):-
	Acc > 1,
	Acc1 is Acc -1,
	drop_every_n(_T,N,Rest,Acc1).


rotate([],_,[]).
rotate(List,0,List).
rotate([H|_T], K, Rotated):-
	K > 0,
	K1 is K-1,
	append(_T,[H],Iteration),
	rotate(Iteration,K1,Rotated).

list_sum([],0).
list_sum([H|_T],Sum):-
	Sum1 is H,
	list_sum(_T,Sum2),
	Sum is Sum1 + Sum2.
max_list([],Acc,Acc).
max_list([H|_T],Max,Acc):-
	(H > Acc ->
		max_list(_T,Max,H);
		max_list(_T,Max,Acc)
		).

max_prefix_sum([],0).

max_prefix_sum(List, Max):-
	findall(Sum,find_prefixes(List,Sum),Sums),
	max_list(Sums,Max,0).

	

find_prefixes(List,Sum):-
	append(Prefix,_,List),
	list_sum(Prefix,Sum).

member_once(_,[]):-fail.
member_once(X, [H|_T]):-
	X = H, !; member_once(X,_T).

grade(Score, Grade):-
	Score >= 90, Grade = a,!;
	Score >= 75, Grade = b,!;
	Score >= 50, Grade = c,!;
	Grade = f.

first_even([],_):-false.
first_even([H|_T], Even):-
	Remainder is H mod 2,
	Remainder = 0,
	Even is H, !;
	first_even(_T,Even).

rev_tr(List, Reversed) :-
    rev_acc(List, [], Reversed).

rev_acc([], Acc, Acc).
rev_acc([H|T], Acc, Reversed) :-
    rev_acc(T, [H|Acc], Reversed).

sum_until_negative(List, Sum):-
	sum_until_negative(List,0,Sum).

sum_until_negative([],0,0).

sum_until_negative([H|_],Acc,Acc):-
	H < 0.

sum_until_negative([H|_T],Acc,Sum):-
    H >= 0,
	Sum1 is H + Acc,
	sum_until_negative(_T,Sum1,Sum).

square(X,SQ):-
	SQ is (X*X).

even(X):-
  Div is X mod 2,
  Div = 0.

map_if(_,_,[],[]).
map_if(Pred, MapPred, [H|_T],Result):-
	map_if(Pred,MapPred,_T,Rest),
	(call(Pred,H) ->
		call(MapPred,H,M),
		append([M],Rest,Result);
		append([H],Rest,Result)
	).

plus(A,B,S):-
	S is A+B.

fold_until(_,_,Acc0,[],Acc0).


fold_until(Pred, Op, Acc0, [H|_T], Acc):-
	(call(Pred,H) ->
		call(Op,Acc0,H,Acc1),
		fold_until(Pred,Op,Acc1,_T,Acc);
		Acc is Acc0
	).

filter_map(_,_,[],[]).
filter_map(Pred, MapPred, [H|_T],Result):-
	filter_map(Pred,MapPred,_T,Rest),
	(call(Pred,H) ->
		call(MapPred,H,M),
		append([M],Rest,Result);
		append([],Rest,Result)
	).


balances(List, Balances):-
	balances(List,0,Balances).

apply_transaction(N,Acc,Balance):-
	Balance is Acc + N.

balances([],_,[]).
balances([H|_T],Acc,[Acc1|Rest]):-
	apply_transaction(H,Acc,Acc1),
	balances(_T,Acc1,Rest).

trans(Current, Symbol, Next).

accepts(_,State,Finals,_,[]):-
	member(State,Finals).

accepts(States, Start, Finals, Transitions, [H|_T]):-
	append(_,[trans(Start,H,Next)|_],Transitions),
	accepts(States,Next,Finals,Transitions,_T).

count_up_to(0):-!.

count_up_to(N):-
	N1 is N-1,
	write(N), nl,
	count_up_to(N1).

compress_runs([],[]).

compress_runs([H|T], Compressed):-
	compress_runs([H|T],H,1,Compressed).

compress_runs([],Prev,Acc,[Prev-Acc]).

compress_runs([H|_T],H,Acc,Res):-
	Acc1 is Acc + 1,
	compress_runs(_T,H,Acc1,Res).

compress_runs([H|_T],Prev,Acc,[Prev-Acc|Rest]):-
	H \= Prev,
	compress_runs(_T,H,1,Rest).

diff_lists(L1, L2, Diff):-
	findall(El,diff_lists_generator(L1,L2,El),Diff).

diff_lists_generator(L1,L2,D):-
	member(D,L1),
	\+((
		append(_,[D|_],L2)
	)).

apply_func(Op,State,Res,_):-
	Op =.. [Func,Arg],
	Func = push,
	append(State,[Arg],Res).

apply_func(Op,State,Prefix,_):-
	Op =.. [Func],
	Func = pop,
	append(Prefix,[_],State).

apply_func(Op,_,Last,History):-
	Op =.. [Func],
	Func = undo,
	append(_,[Last],History).

apply_func(Op,_,[],[]):-
	Op =.. [Func],
	Func = undo.

apply_ops(Ops, FinalState, History):-
	apply_ops_acc(Ops,[],FinalState,[],History,0).

apply_ops_acc([],Start,Start,History,History,_).

apply_ops_acc([Opp|_T],Start,FinalState,HistoryAcc,History,Count):-
	apply_func(Opp,Start,Next,HistoryAcc),
	(Count \= 0 -> append(HistoryAcc,[Start],HistoryAcc2); append(HistoryAcc,[],HistoryAcc2)),
	apply_ops_acc(_T,Next,FinalState,HistoryAcc2,History,1).

% is_bst(nil).
% is_bst(t(V,L,R)):-
% 	is_bst_left(V,L,no_max),
% 	is_bst_right(V,R,no_min).

% is_bst_left(_,nil,_):-!.

% is_bst_left(Val,t(V,Left,Right),Max):-
% 	Val > V,
% 	(Max = no_max ->
% 		!;
% 		Val < Max
% 	), 
% 	is_bst_left(V,Left,Val),
% 	is_bst_right(V,Right,Val).

% is_bst_right(_,nil,_):-!.
% is_bst_right(Val,t(V,Left,Right),Min):-
% 	Val < V,
% 		(Min = no_min ->
% 		!;
% 		Val > Min
% 	), 
	
% 	is_bst_left(V,Left,Val),
% 	is_bst_right(V,Right,Val).
% WRONG IMPLEMENTATION

change(Coins, Money, Selection):-
length(Selection,_N),
change(Coins,Money,Selection,0),
!.

change(_,Money,[],Money).

change(Coins,Money,[C|Rest],Acc):-
	member(C,Coins),
	Acc1 is Acc + C,
	Acc1 =< Money,
	change(Coins,Money,Rest,Acc1).

edge(a, b).
edge(b, c).
edge(c, a).
edge(1, 2).
edge(2, 3).
edge(3, 4).
edge(x, x).
edge(start, mid).
edge(mid, loop_start).
edge(loop_start, loop_end).
edge(loop_end, loop_start).
edge(center, left).
edge(left, center).
edge(center, right).
edge(right, center).
edge(n1, n2).

has_cycle(StartNode):-
	has_cycle(StartNode,StartNode,[]).

has_cycle(Start,Start,_Visited):-
	edge(Start,Start),!.

has_cycle(Start,Start,[Start,_|_Visited]):-!.

has_cycle(Start,End,Visited):-
	edge(Start,Next),
	\+(member(Next,Visited)),
	has_cycle(Next,End,[Next|Visited]).

is_list([]).
is_list([_H|_]).

flatten_depth(NestedList, FlatList, MaxDepth):-
	flatten_depth_rec(NestedList,FlatList,MaxDepth).


flatten_depth_rec([],[],_).

flatten_depth_rec([H|_T],Flat,MaxDepth):-
	is_list(H),
	MaxDepth > 0,
	!,
	MaxDepth1 is MaxDepth -1,
	flatten_depth_rec(H,FlatHead,MaxDepth1),
	flatten_depth_rec(_T,FlatTail,MaxDepth),
	append(FlatHead,FlatTail,Flat).

flatten_depth_rec([H|T],[H|FlatT],MaxDepth):-
	flatten_depth_rec(T,FlatT,MaxDepth).





get_index(Elem,List,Indx):-
	get_index(Elem,List,0,Indx).

get_index(H,[H|_T],Acc,Acc):-!.
get_index(Elem,[_|_T],Acc,Final):-
	Acc1 is Acc + 1,
	get_index(Elem,_T,Acc1,Final).

flatten([],[]).

flatten([H|T],[H|Rest]):-
	\+ is_list(H),
	flatten(T,Rest).

flatten([H|T],Res):-
	is_list(H),
	!,
	flatten(H,HeadFlat),
	flatten(T,TFlat),
	append(HeadFlat,TFlat,Res).

generate(N,[],N):-!.

generate(N,[Acc|Rest],Acc):-
	Acc1 is Acc + 1,
	generate(N,Rest,Acc1).

final(N,Board):-
	generate(N,Prefix,1),
	append(Prefix,[0],Board).

move(Board,N,NextBoard,up):-
	get_index(0,Board,Index),
	Index >= N,
	TargetIndex is Index - N,
	get_index(Target,Board,TargetIndex),
	swap(Board,Target,NextBoard).

move(Board,N,NextBoard,left):-
	get_index(0,Board,Index),
	Index mod N =\= 0,
	TargetIndex is Index - 1,
	get_index(Target,Board,TargetIndex),
	swap(Board,Target,NextBoard).

move(Board,N,NextBoard,right):-
	get_index(0,Board,Index),
	Index < N*N-1,
	TargetIndex is Index + 1,
	get_index(Target,Board,TargetIndex),
	swap(Board,Target,NextBoard).

move(Board,N,NextBoard,down):-
	get_index(0,Board,Index),
	Index < N*(N-1),
	TargetIndex is Index + N,
	get_index(Target,Board,TargetIndex),
	swap(Board,Target,NextBoard).

sliding_puzzle([H|T], Moves):-
	length(H,N),
	Size is N*N,
	final(Size,FinalBoard),
	flatten([H|T],StartBoard),
 	solve_bfs(N,StartBoard,FinalBoard,Moves).

dfs_puzzle(_,Goal,Goal,[],_).

dfs_puzzle(N,Start,Goal,[Move|Moves],Path):-
	move(Start,N,Next,Move),
	\+ member(Next,Path),
	dfs_puzzle(N,Next,Goal,Moves,[Next|Path]).

reverse([],[]).
reverse([H|_T],Result):-
	reverse(_T,Rest),
	append(Rest,[H],Result).


solve_bfs(N, Start, Goal, Moves) :-
    bfs_puzzle(N, [[Start, []]], Goal, Moves, [Start]).

% 1. Base Case: The head of the queue is the Goal state.
% We simply reverse the path (accumulated in reverse) and return it.
bfs_puzzle(_, [[Goal, Path] | _], Goal, Moves, _Visited) :-
    reverse(Path, Moves),!.

% 2. Recursive Step
% We ignore the 'Moves' variable here; it is only unified in the Base Case.
bfs_puzzle(N, [[State, Path] | RestQueue], Goal, FinalMoves, Visited) :-
    
    % Find all valid children: returns list of [[NextState, NewPath], ...]
    findall([Next, [Dir|Path]], (
        move(State, N, Next, Dir),
        \+ member(Next, Visited)       % Check if already visited
    ), Children),

    % Extract just the states from Children to add to Visited list
    extract_states(Children, NewStates),
    append(Visited, NewStates, NewVisited),

    % Append children to the END of the queue (BFS logic)
    append(RestQueue, Children, NewQueue),
    
    % Recurse
    bfs_puzzle(N, NewQueue, Goal, FinalMoves, NewVisited).

% Helper to get states out of the [State, Path] pairs for the Visited list
extract_states([], []).
extract_states([[State, _] | T], [State | Rest]) :-
    extract_states(T, Rest).

swap(List,Target,Result):-
	get_index(0,List,ZeroIndex),
	get_index(Target,List,TargetIndex),
	TargetIndex < ZeroIndex,
	!,
	append(PrefixTarget,[Target|SuffixTarget],List),
	append(PrefixZero,[0|SuffixZero],SuffixTarget),
	append(PrefixTarget,[0|PrefixZero],Medium),
	append(Medium,[Target|SuffixZero],Result).

swap(List,Target,Result):-
	get_index(0,List,ZeroIndex),
	get_index(Target,List,TargetIndex),
	TargetIndex > ZeroIndex,
	append(PrefixTarget,[0|SuffixTarget],List),
	append(PrefixZero,[Target|SuffixZero],SuffixTarget),
	append(PrefixTarget,[Target|PrefixZero],Medium),
	append(Medium,[0|SuffixZero],Result).









