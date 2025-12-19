list_size([],0).

list_size([_|_T],Size):- 
list_size(_T,Size1),
Size is 1+Size1.

list_sum([],0).

list_sum([H|_T],S):-
list_sum(_T,S1),
S is H + S1.

list_prod([],1).

list_prod([H|_T],S):-
list_prod(_T,S1),
S is H * S1.

inner_product([],[],0).

inner_product([XI|_T1],[YI|_T2],Result):-
Mul is XI*YI,
inner_product(_T1,_T2,Result1),
Result is Mul + Result1.

count(_,[],0).

count(X,[H|_T],Count):-
count(X,_T,Count1),
(X =:= H -> R1 is 1; R1 is 0),
Count is R1 + Count1.

invert([],[]).
invert([H|_T],Result):-
invert(_T,Result1),
append(Result1,[H],Result2),
Result = Result2.

del_one(_,[],[]).

del_one(X,[H|_T],Result):-
(X =:= H -> R1 is 1; R1 is 0),
(R1 =:= 1 -> Result1 = _T; append([H],Result2,Result1), del_one(X,_T,Result2)),
Result = Result1.

del_all(_,[],[]).
del_all(X,[H|_T],Result):-
    (X =:= H -> R1 is 1; R1 is 0),
    (R1 =:= 1 -> del_all(X,_T,Result1); append([H],Result2,Result1), del_all(X,_T,Result2)),
    Result = Result1.



