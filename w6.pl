
map(_,[],[]).

map(Pred,[H|_T],[R|Result]):-
	call(Pred,H,R),
	map(Pred,_T,Result).

fold(_,StartValue,[],StartValue).

fold(Pred, StartValue, [H|_T], FinalValue):-
	call(Pred,StartValue,H,Result),
	fold(Pred,Result,_T,FinalValue).

separate([],_,[],[]).

separate([H|T], Pred, Yes, No) :-
    (   call(Pred, H)
    ->  Yes = [H|Yes1],
        separate(T, Pred, Yes1, No)
    ;   No  = [H|No1],
        separate(T, Pred, Yes, No1)
    ).

take_while(_,[],[],[]).
take_while(Pred, [H|_T], Front, Back):-
	(   call(Pred, H)
    ->  Front = [H|Yes1],
        take_while(Pred,_T, Yes1, Back)
    ;   Back  = [H|_T], Front = [],
        !
    ).

even(X):- 0 =:= X mod 2.
double(X, Y):- Y is X*2.

ask_execute :-
    write('Insert the goal to execute'), nl,
    read(Goal),
    call(Goal).

my_functor(Term, Name, Arity) :-
    nonvar(Term),
    !,
    Term =.. [Name | Args],
    length(Args, Arity).

my_functor(Term, Name, Arity) :-
    var(Term),
    integer(Arity),
    Arity >= 0,
    length(Args, Arity),
    Term =.. [Name | Args].

my_arg(Index,Term,Arg):-
	Term =.. [_|Args],
	at_pos(Index,Args,Arg).

at_pos(1,[H|_],H).

at_pos(Index,[_|_T],Arg):-
	Index > 1,
	Index1 is Index - 1,
	at_pos(Index1,_T,Arg).

univ(Term, [Name | Args]) :-
    nonvar(Term),
    !,
    my_functor(Term, Name, Arity),
    build_args(1, Arity, Term, Args).

univ(Term, [Name | Args]) :-
    var(Term),
    length(Args, Arity),
    my_functor(Term, Name, Arity),
    fill_args(1, Args, Term).

build_args(I, Arity, _Term, []) :-
    I > Arity,
    !.
build_args(I, Arity, Term, [A|As]) :-
    my_arg(I, Term, A),
    I1 is I + 1,
    build_args(I1, Arity, Term, As).


fill_args(_I, [], _Term).
fill_args(I, [A|As], Term) :-
    my_arg(I, Term, A),
    I1 is I + 1,
    fill_args(I1, As, Term).

:- op(700, xfx, univ).

node(Value, Left,Right).

tree(null).
tree(node(Value,Left,Right)):-
    tree(Left),
    tree(Right).

tree_size(null,0).

tree_size(node(Value,Left,Right),Size):-
    Value \= null,
    tree_size(Left, SizeLeft),
    tree_size(Right, SizeRight),
    Size is 1 + SizeLeft + SizeRight.


test_tree(node(3, node(1, null, null),
    node(7, node(5,null,null),
    node(9,null,null) ) )).


tree_map(_, null, null).

tree_map(Pred, node(Value,Left,Right), node(Called,CalledLeft,CalledRight)):-
    call(Pred,Value,Called),
    tree_map(Pred,Left,CalledLeft),
    tree_map(Pred,Right,CalledRight).

tree_value_at_level_(node(Value, _, _), Value, 0).
tree_value_at_level_(node(_, Left, _), Value, Level) :-
    tree_value_at_level_(Left, Value, L1),
    Level is L1 + 1.
tree_value_at_level_(node(_, _, Right), Value, Level) :-
    tree_value_at_level_(Right, Value, L1),
    Level is L1 + 1.

tree_value_at_level(Tree, Value, Level) :-
    nonvar(Value),
    !,
    (   tree_value_at_level_(Tree, Value, Level)
    ->  true
    ;   Level = -1
    ).

tree_value_at_level(Tree, Value, Level) :-
    nonvar(Level),
    Level >= 0,
    tree_value_at_level_(Tree, Value, Level).

:- op(400, xfx, then).
:- op(300, xfx, else).
:- op(500,fx,if).

:- op(500,xfx,exists_in).

exists_in(X,L) :-
    member(X,L).


:- op(500, fx, append).
:- op(400, xfx, to).
:- op(300, xfx, results_in).

append A to B results_in C :-
    append(A, B, C).

:- op(500,fx,remove).
:- op(400, xfx, from).

remove Elem from List results_in Result:-
    removeElement(Elem,List,Result).

removeElement(_,[],[]).
removeElement(Elem,[H|_T],Result):-
    removeElement(Elem,_T,Result2),
    (Elem = H ->
        append([],Result2,Result);
        append([H],Result2,Result)
        ).

% sum_list([],0).

% sum_list([H|_T],R):-
%     sum_list(_T,R1),
%     R is R1 + H.

% find_one([1|_T],0).
% find_one([_|_T],Pile):-
%     find_one(_T,PileIndex),
%     Pile is PileIndex + 1.

% finished_state(State,(Pile,1)):-
%     sum_list(State,Sum),
%     Sum = 1,
%     find_one(State,Pile).

between(Min, Max, Min) :-
    Min =< Max.
between(Min, Max, Var) :-
    Min < Max,
    Next is Min + 1,
    between(Next, Max, Var).

% pile_index(State, Index) :-
%     length(State, N),
%     Max is N - 1,
%     between(0, Max, Index).

% transition(State,EndState,(Pile,Take)):-
%     pile_index(State, Pile),
%     at_index(State, Pile, Amount),
%     Amount > 0,
%     between(1, Amount, Take),
%     NewAmount is Amount - Take,
%     replace_at_index(State, Pile, EndState, NewAmount).

% replace_at_index([_|_T],0,[Replace|_T],Replace).
% replace_at_index([H|_T],Index,[H|Result],Replace):-
%     Index1 is Index - 1,
%     replace_at_index(_T,Index1,Result,Replace).

% at_index([H|_T],0,H).
% at_index([_|_T],Index,R):-
%     Index1 is Index - 1,
%     at_index(_T,Index1,R).

% terminal(State) :-
%     sum_list(State, 0).

% losing(State) :-
%     terminal(State).

% losing(State) :-
%     \+ winning(State).

% winning(State) :-
%     transition(State, NextState, _),
%     losing(NextState).

% winning_move(State, Move) :-
%     transition(State, NextState, Move),
%     losing(NextState).
% my terrible way

:- op(500,xfx,xor).

nim_sum([], 0).
nim_sum([H|T], R) :-
    nim_sum(T, R1),
    R is H xor R1.

at_index([H|_], 0, H).
at_index([_|T], Index, R) :-
    Index1 is Index - 1,
    at_index(T, Index1, R).

index(State, I) :-
    length(State, N),
    N1 is N - 1,
    between(0, N1, I).

winning_move(State, (Pile, Take)) :-
    nim_sum(State, NimSum),
    NimSum =\= 0,
    index(State, Pile),
    at_index(State, Pile, Size),
    NewSize is Size xor NimSum,
    NewSize < Size,
    Take is Size - NewSize.