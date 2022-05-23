gcd(X, X, X).
gcd(X, Y, Z) :- X > Y, R is X-Y, gcd(R, Y, Z).
gcd(X, Y, Z) :- X < Y, R is Y-X, gcd(X, R, Z).

fact(0, 1).
fact(N, F) :- 
    N > 0, N1 is N - 1, 
    fact(N1, F1), 
    F is F1 * N.

fact1(0, FF, FF).
fact1(N, FP, FF) :- 
    N > 0, N1 is N - 1, 
    FP1 is FP * N, 
    fact1(N1, FP1, FF).

fact1_pretty(N, F) :- fact1(N, 1, F).

forLoop(In, In, 0) :- !.
forLoop(In, Sum, I) :-
    NextI is I - 1,
    Intermediate is In + I,
    forLoop(Intermediate, Sum, NextI).

lcm(X, Y, Z) :- gcd(X, Y, GCD), Z is X * Y / GCD.

fib(0, 0).
fib(1, 1).
fib(N, R) :- 
    N1 is N - 1, 
    N2 is N-2, 
    fib(N1, R1), 
    fib(N2, R2), 
    R is R1 + R2.

fib_2(1, Acc1, Acc2, R) :- R is Acc1 + Acc2.
fib_2(N, Acc1, Acc2, R) :- 
    N1 is N-1, 
    Acc3 is Acc1 + Acc2, 
    fib_2(N1, Acc2, Acc3, R).

edge_rule(L1, L2, L3) :- L1 + L2 > L3.
triangle_rule(A, B, C) :- 
    edge_rule(A, B, C), 
    edge_rule(B, C, A), 
    edge_rule(A, C, B).

solve(A, B, C, X) :- 
    D is B * B - 4 * A * C,
    X is (-B + sqrt(D)) / (2 * A).
solve(A, B, C, X) :- 
    D is B * B - 4 * A * C,
    X is (-B - sqrt(D)) / (2 * A).