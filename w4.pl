%% ---------- data for cut tests ----------
data(one).
data(two).
data(three).

cut_test_a(X) :- data(X).
cut_test_a('five').

cut_test_b(X) :- data(X), !.
cut_test_b('five').

cut_test_c(X, Y) :- data(X), !, data(Y).
cut_test_c('five', 'five').



print_full_list(List):-
    print('['),
	print_elems(List),
	print(']').

print_elems([X]):-
    print(X),
    !.

print_elems([H|_T]):-
	print(H),
    print(', '),
	print_elems(_T).

print_list(L):-
	length(L,Len),
	Len =< 11,
	print_full_list(L), 
	!.

print_list(L):-
	append([A,B,C],Rest,L),
	append(Middle,[G,H,I],Rest),
	length(Middle, MidLen),
    Start is MidLen // 2 - 1,
    Start >= 0,
    length(Prefix, Start),
    append(Prefix, [D,E,F|_], Middle),
    print('['),
    print_elems([A,B,C]),
    print(', ..., '),
    print_elems([D,E,F]),
    print(', ..., '),
    print_elems([G,H,I]),
    print(']'), 
	!.

print_list(L,S,Sep,E):-
	print_full_list(L,S,Sep,E).

print_full_list(List,S,Sep,E):-
    print(S),
	print_elems(List,Sep),
	print(E).

print_elems([X],_):-
    print(X),
    !.

print_elems([H|_T],Sep):-
	print(H),
    print(Sep),
	print_elems(_T,Sep).



print_matrix([H]):-print_list(H), !.

print_matrix([H|_T]):-
	print_list(H),
	print('\n'),
	print_matrix(_T),
	!.

print_numbered_matrix(L) :-
    length(L, Size),
    number_chars(Size, SizeChars),
    length(SizeChars, Width),
    print_numbered_matrix(L, 1, Width).

print_numbered_matrix([], _, _).
print_numbered_matrix([H|T], Index, Width) :-
    print_line_number(Index, Width),
    print_list(H),
    nl,
    Index1 is Index + 1,
    print_numbered_matrix(T, Index1, Width).


print_line_number(Index, Width) :-
    number_chars(Index, Chars),
    length(Chars, Len),
    Spaces is Width - Len,
    print_spaces(Spaces),
    print(Index),
    print(' ').

print_spaces(0).
print_spaces(N) :-
    N > 0,
    print(' '),
    N1 is N - 1,
    print_spaces(N1).