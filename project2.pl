anywhere(_, _).

consecutive(X,Y,Board) :-
	append(_,[X,Y|_], Board).

next_to(X,X,_).
next_to(X,Y,Board) :-
	consecutive(X,Y,Board).
next_to(X,Y,Board) :-
	consecutive(Y,X,Board).

one_space(X, X, _) :- !.
one_space(X, Y, Board) :-
    X \= Y,
    append(_, [X, _, Y | _], Board).
one_space(X, Y, Board) :-
    X \= Y,
    append(_, [Y, _, X | _], Board).

across(X, X, _) :- !.
across(X, Y, [A,B,_,D,E,F]) :-
    X \= Y,
    ((member(X, [A,B]), member(Y, [D,E,F])) ;
     (member(X, [D,E,F]), member(Y, [A,B]))).

same_edge(X,X,_).
same_edge(X, Y, [A,B,_,D,E,F]) :-
    X \= Y,
    ((member(X, [A,B]), member(Y, [A,B])) ;
     (member(X, [D,E,F]), member(Y, [D,E,F]))).

position(X, L, Board):-
	pos_map(Pos,X,Board),
	member(Pos, L).

pos_map(1, A, [A,_,_,_,_,_]).
pos_map(2, B, [_,B,_,_,_,_]).
pos_map(3, C, [_,_,C,_,_,_]).
pos_map(4, D, [_,_,_,D,_,_]).
pos_map(5, E, [_,_,_,_,E,_]).
pos_map(6, F, [_,_,_,_,_,F]).

%% 12 solutions
example(1, [ next_to(white,orange),
	next_to(black,black),
	across(yellow,orange),
	next_to(green,yellow),
	position(blue,[1,2,6]),
	across(yellow,blue) ]).

%% 1 solution
example(2, [ across(white,yellow),
	position(black,[1,4]),
	position(yellow,[1,5]),
	next_to(green, blue),
	same_edge(blue,yellow),
	one_space(orange,black) ]).

%% no solutions (5 constraints are satisfiable)
example(3, [ across(white,yellow),
	position(black,[1,4]),
	position(yellow,[1,5]),
	same_edge(green, black),
	same_edge(blue,yellow),
	one_space(orange,black) ]).

%% same as above, different order of constraints
example(4, [ position(yellow,[1,5]),
	one_space(orange,black),
	same_edge(green, black),
	same_edge(blue,yellow),
	position(black,[1,4]),
	across(white,yellow) ]).

apply_constraint(Board,Constraint):-
	call(Constraint,Board).

apply_all_constraints([], _).
apply_all_constraints([Constraint|Rest], Board) :-
	apply_constraint(Board,Constraint),
	apply_all_constraints(Rest, Board).

solve(Constraints,[A,B,C,D,E,F]) :-
	Colors = [green, yellow, blue, orange, white, black],
	member(A, Colors),
    member(B, Colors), B \= A,
    member(C, Colors), C \= A, C \= B,
    member(D, Colors), D \= A, D \= B, D \= C,
    member(E, Colors), E \= A, E \= B, E \= C, E \= D,
    member(F, Colors), F \= A, F \= B, F \= C, F \= D, F \= E,
    apply_all_constraints(Constraints, [A,B,C,D,E,F]).

count_satisfied([], _, 0).
count_satisfied([C|Cs], Board, Count) :-
    count_satisfied(Cs, Board, RestCount),
    (apply_constraint(Board, C), !, Count is RestCount + 1 ; Count = RestCount).

	valid_board([A,B,C,D,E,F]) :-
		Colors = [green, yellow, blue, orange, white, black],
		member(A, Colors),
		member(B, Colors), B \= A,
		member(C, Colors), C \= A, C \= B,
		member(D, Colors), D \= A, D \= B, D \= C,
		member(E, Colors), E \= A, E \= B, E \= C, E \= D,
		member(F, Colors), F \= A, F \= B, F \= C, F \= D, F \= E.
	
	max_satisfied(Constraints, MaxCount) :-
		findall(Count, 
				(valid_board(Board), count_satisfied(Constraints, Board, Count)), 
				Counts),
		max_in_list(Counts, MaxCount).

best_score(Constraints, Score) :-
    length(Constraints, Total),
    max_satisfied(Constraints, MaxSatisfied),
    Score is MaxSatisfied - Total.

max_in_list([X], X).
max_in_list([X|Xs], Max) :-
    max_in_list(Xs, MaxRest),
    (X > MaxRest -> Max = X ; Max = MaxRest).