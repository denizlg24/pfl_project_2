
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