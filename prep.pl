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









