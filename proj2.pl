consecutive(X,Y,Board) :-
append(_Prefix,[X,Y|_Suffix], Board).

next_to(X,X,_).

next_to(X,Y,[A,B,C,D,E,F]) :-
	consecutive(X,Y,[A,B,C,D,E,F]).

next_to(X,Y,[A,B,C,D,E,F]) :-
	consecutive(Y,X,[A,B,C,D,E,F]).

circular_next_to(X, Y, [A,B,C,D,E,F]):-
	next_to(X,Y,[A,B,C,D,E,F]).

circular_next_to(X,Y,[X,_,_,_,_,Y]).
circular_next_to(X,Y,[Y,_,_,_,_,X]).

at_corner(A, [A,_,_,_,_,_]).
at_corner(F, [_,_,_,_,_,F]).

not_next_to(X, Y, [A,B,C,D,E,F]):-
	get_index(X,[A,B,C,D,E,F],XIndex),
	get_index(Y,[A,B,C,D,E,F],YIndex),
	XIndex \== YIndex,
	Diff is abs(XIndex-YIndex),
	Diff > 1.

between_colors(X, Middle, Y, [A,B,C,D,E,F]):-
	append(_Prefix,[X,Middle,Y|_Suffix],[A,B,C,D,E,F]).

anywhere(_X,_Board).

spaced(X,Y,Board):-
	append(_Prefix,[X,_,Y|_Suffix],Board).

one_space(X, Y, [A,B,C,D,E,F]):-
	spaced(X,Y,[A,B,C,D,E,F]).

one_space(X, Y, [A,B,C,D,E,F]):-
	spaced(Y,X,[A,B,C,D,E,F]).

across(X, Y, [A,B,_C,D,E,F]):-
	member(X,[A,B]),
	member(Y,[D,E,F]).

across(X, Y, [A,B,_C,D,E,F]):-
	member(Y,[A,B]),
	member(X,[D,E,F]).

same_edge(X,X,_).

same_edge(X, Y, [A,B,_C,_D,_E,_F]):-
	member(Y,[A,B]),
	member(X,[A,B]).

same_edge(X, Y, [_A,_B,_C,D,E,F]):-
	member(Y,[D,E,F]),
	member(X,[D,E,F]).

get_index(X, [X|_], 1).
get_index(X, [_|Rest], Index) :-
    get_index(X, Rest, Index0),
    Index is Index0 + 1.

position(X, L, Board):-
	pos_map(Pos,X,Board),
	member(Pos, L).

pos_map(1, A, [A,_,_,_,_,_]).
pos_map(2, B, [_,B,_,_,_,_]).
pos_map(3, C, [_,_,C,_,_,_]).
pos_map(4, D, [_,_,_,D,_,_]).
pos_map(5, E, [_,_,_,_,E,_]).
pos_map(6, F, [_,_,_,_,_,F]).


solve(Constraints, [A,B,C,D,E,F]):-
	member(green,[A,B,C,D,E,F]),
	member(yellow,[A,B,C,D,E,F]),
	member(blue,[A,B,C,D,E,F]),
	member(orange,[A,B,C,D,E,F]),
	member(white,[A,B,C,D,E,F]),
	member(black,[A,B,C,D,E,F]),
	apply_constraints(Constraints,[A,B,C,D,E,F]).

apply_constraints([],_Board).

apply_constraints([H|_T],Board):-
	call(H,Board),
	apply_constraints(_T,Board).

example(1, [ next_to(white,orange),
next_to(black,black),
across(yellow,orange),
next_to(green,yellow),
position(blue,[1,2,6]),
across(yellow,blue) ]).

example(2, [ across(white,yellow),
position(black,[1,4]),
position(yellow,[1,5]),
next_to(green, blue),
same_edge(blue,yellow),
one_space(orange,black) ]).

example(3, [ across(white,yellow),
position(black,[1,4]),
position(yellow,[1,5]),
same_edge(green, black),
same_edge(blue,yellow),
one_space(orange,black) ]).

example(4, [ position(yellow,[1,5]),
one_space(orange,black),
same_edge(green, black),
same_edge(blue,yellow),
position(black,[1,4]),
across(white,yellow) ]).

count_satisfied([], _, 0).
count_satisfied([C|Cs], Board, Count) :-
    count_satisfied(Cs, Board, RestCount),
    (call(C,Board), !, Count is RestCount + 1 ; Count = RestCount).

valid_board([A,B,C,D,E,F]):-
	member(green,[A,B,C,D,E,F]),
	member(yellow,[A,B,C,D,E,F]),
	member(blue,[A,B,C,D,E,F]),
	member(orange,[A,B,C,D,E,F]),
	member(white,[A,B,C,D,E,F]),
	member(black,[A,B,C,D,E,F]).

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