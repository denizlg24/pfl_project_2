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


