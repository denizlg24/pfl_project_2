factorial(0,1).
factorial(N,F):- N>0,
N1 is N-1,
factorial(N1,F1),
F is N * F1.

tail_factorial(0,1).
tail_factorial(N,F):- factorial_acc(N,1,F).

factorial_acc(0, Acc, Acc).
factorial_acc(N, Acc, F) :-
    N > 0,
    Acc1 is Acc * N,
    N1 is N - 1,
    factorial_acc(N1, Acc1, F).


sum_rec(0,0).
sum_rec(N, Sum) :- N>0,
N1 is N-1,
sum_rec(N1,Sum1),
Sum is Sum1 + N.

tail_sum(0,0).
tail_sum(N, Sum):- tail_sum_acc(N,0,Sum).

tail_sum_acc(N,Acc,Sum):- N>0,
N1 is N-1,
Acc1 is Acc + N,
tail_sum_acc(N1,Acc1,Sum).
 

pow_rec(_,0,1).
pow_rec(X,N,Result):- N>0,
N1 is N-1,
pow_rec(X,N1,Result1),
Result is Result1 * X.

square_rec(N, S):- pow_rec(N,2,S).

fibonacci(0,0).
fibonacci(1,1).
fibonacci(N, F):- N>1,
N1 is N-1,
fibonacci(N1,F1),
N2 is N-2,
fibonacci(N2,F2),
F is F1 + F2.

collatz(1, 0).
collatz(2,1).

collatz(N, Steps):- N>2,
N1 is N mod 2,
( N1 =:= 0 -> N2 is N // 2; N2 is 3 * N + 1),
collatz(N2, Steps1),
Steps is Steps1 + 1.

is_prime(X):- X>1,
\+ has_factor(X,2).

has_factor(X,Y):- Y*Y =< X,
X mod Y =:= 0.

gcd(X,0,X).

gcd(X,Y,G):- Y > 0,
Y1 is X mod Y,
gcd(Y,Y1,G1),
G is G1.

lcm(X,Y,M):-
MUL is X*Y,
gcd(X,Y,G1),
M is MUL/G1.

