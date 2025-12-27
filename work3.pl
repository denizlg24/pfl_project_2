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

del_all_list([],List1,List1).

del_all_list([H|_T], List1, Result):-
    del_all(H,List1,Result1),
    del_all_list(_T,Result1,Result). 

del_dups([],[]).

del_dups([H|_T], [H|List2]):-
    del_all(H,_T,Result1),
    del_dups(Result1,List2).

list_perm([],[]).

list_perm(L1,[H|T]):-
    del_one(H,L1,L1_without_H),
    list_perm(L1_without_H,T).

replicate(0,_,[]).
replicate(N, X, Result):-
    N1 is N - 1,
    replicate(N1,X,Result1),
    append([X],Result1,Result). 

intersperse(_,[H],[H]).
intersperse(Elem, [H|_T], Result):-
    intersperse(Elem,_T,Result1),
    append([H,Elem],Result1,Result).

insert_elem(0,List1,X,Result):-
    append([X],List1,Result).

insert_elem(N, [H|_T], X, Result):-
    N1 is N - 1,
    insert_elem(N1,_T,X,Result1),
    Result = [H|Result1].


delete_elem(0, [Elem|Tail], Elem, Tail).
delete_elem(Index, [Head|Tail], Elem, [Head|NewTail]) :-
    NextIndex is Index - 1,
    delete_elem(NextIndex, Tail, Elem, NewTail).


replace(List, N, Old, New, Result):-
    delete_elem(N,List,Old,Deleted),
    insert_elem(N,Deleted,New,Result).

list_append([],L2,L2).
list_append(L1,[],L1).
list_append([H|_T],L2,[H|Result1]):-
    list_append(_T,L2,Result1).

list_member(X,[X]).
list_member(X,[H|_T]):- append([X],_T,[H|_T]); list_member(X,_T).

list_last(List, Last) :-
    append(_, [Last], List).

list_nth(N,List,Elem):-
    N1 is N - 1,
    length(Prefix,N1),
    append(Prefix,[Elem|_T],List).

list_append([],[]).

list_append([H|_T],Result):-
    list_append(_T,Result1),
    list_append(H,Result1,Result).

list_del(List,Elem,Res):-
    append(Prefix,[Elem|Suffix],List),
    append(Prefix,Suffix,Res).

list_before(First, Second, List):-
    append(_, [First|Rest], List),
    append(_, [Second|_], Rest).

list_replace_one(X,Y,List1,List2):-
  append(Prefix,[X|Suffix],List1),
  append(Prefix,[Y|Suffix],List2).

list_repeated(X,List):-
    append(_,[X|Rest],List),
    append(_,[X|_],Rest).

list_slice(List1, Index, Size, List2):-
    Stop is Index - 1 ,
    append(_Before,_Rest,List1),
    length(_Before,Stop),
    append(List2,_,_Rest),
    length(List2,Size).

list_shift_rotate(List1, N, List2):-
    length(Rotated,N),
    append(Rotated,Suffix,List1),
    append(Suffix,Rotated,List2).