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

list_to(1,[1]).

list_to(N,List):-
    N > 1,
    N1 is N-1,
    list_to(N1,Result),
    append(Result,[N],List).

list_from_to(End,End,[End]).

list_from_to(Start,End,List):-
    Start < End,
    N is Start + 1,
    list_from_to(N,End,Result),
    append([Start],Result,List).

list_from_to(Start,End,List):-
    Start > End,
    N is Start - 1,
    list_from_to(N,End,Result),
    append([Start],Result,List).

list_from_to_step(End,End,_,[End]).
list_from_to_step(Start, End, Step, List):-
    Start < End,
    N is Start + Step,
    (N > End ->
        Result = [];
        list_from_to_step(N,End,Step,Result)
    ),
    append([Start],Result,List).

list_from_to_step(Start, End, Step, List):-
    Start > End,
    N is Start - Step,
    (N < End ->
        Result = [];
        list_from_to_step(N,End,Step,Result)
    ),
    append([Start],Result,List).

is_prime(X):- X>1,
\+ has_factor(X,2).

has_factor(X,Y):- Y*Y =< X,
X mod Y =:= 0.

primes(N,List):- 
    list_from_to(2,N,Search),
    include(is_prime, Search, List).

fibonacci(0,0).

fibonacci(1,1).

fibonacci(N, F):- N>1,
    N1 is N-1,
    fibonacci(N1,F1),
    N2 is N-2,
    fibonacci(N2,F2),
    F is F1 + F2.

fibs(N,List):-
    list_from_to(0,N,Search),
    maplist(fibonacci, Search, List).


rle([], []).
rle(List, [Elem-Count | Rest]) :-
    List = [Elem | _],
    group(=(Elem), List, Group, Tail),
    length(Group, Count),
    rle(Tail, Rest).

unrle([],[]).

unrle([Elem-Count | Tail],List):-
    replicate(Count,Elem,Result),
    unrle(Tail,Result2),
    append(Result,Result2,List).

is_ordered([]).
is_ordered([_]).
is_ordered([A|[B|T]]):-
    A =< B,
    is_ordered([B|T]).

insert_ordered(Value, [], [Value]).
insert_ordered(Value, [Head|Tail], [Value, Head|Tail]) :-
    Value =< Head.
insert_ordered(Value, [Head|Tail], [Head|NewTail]) :-
    Value > Head,
    insert_ordered(Value, Tail, NewTail).

insert_sort([],[]).

insert_sort([Head|Tail], SortedList) :-
    insert_sort(Tail, SortedTail),
    insert_ordered(Head, SortedTail, SortedList).

xor(0,0,0).
xor(1,0,1).
xor(1,1,0).
xor(0,1,1).

and(0,_,0).
and(1,1,1).
and(1,0,0).

or(0,0,0).
or(0,1,1).
or(1,1,1).
or(1,0,1).

full_adder(A,B,Cin,S,Cout):-
    xor(A,B,AB),
    xor(AB,Cin,S),
    and(A,B,AandB),
    and(Cin,AB,CAB),
    or(AandB,CAB,Cout).

ripple_adder(List1,List2,Result):-
    ripple_adder_with_carry(List1,List2,0,Result).

ripple_adder_with_carry([], [], Cin, [Cin]).

ripple_adder_with_carry([A|T1], [B|T2], Cin, [S|Result]) :-
    full_adder(A, B, Cin, S, Cout),
    ripple_adder_with_carry(T1, T2, Cout, Result).