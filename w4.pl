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

%% ---------- max/4 using cuts (keeps deterministic answers) ----------
max(A, B, C, A) :-
    A >= B,
    A >= C,
    !.
max(_, B, C, B) :-
