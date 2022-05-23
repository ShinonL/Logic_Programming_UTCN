% Sum for shallow lists
sum([], 0).
sum([H|T], R) :- sum(T, R1), R is R1 + H.

% Sum for deep lists
sum_deep([], 0).
sum_deep([H|T], R) :- 
    atomic(H), !, sum_deep(T, R1), R is R1 + H.
sum_deep([H|T], R) :- 
    sum_deep(H, R1), sum_deep(T, R2), R is R1 + R2.

max(A, B, B) :- A < B, !.
max(A, _, A).

depth([], 1).
depth([H|T], R) :- atomic(H), !, depth(T, R).
depth([H|T], R) :- 
    depth(H, R1), depth(T, R2), R3 is R1 + 1, max(R3, R2, R).

flatten([], []).
flatten([H|T], [H|R]) :- atomic(H), !, flatten(T, R).
flatten([H|T], R) :- 
    flatten(H, R1), flatten(T, R2), append(R1, R2, R).

% al treilea argument e un flag care marcheaza elem daca 
% e primul element din lista
heads3([], [], _).
heads3([H|T], [H|R], 1) :- atomic(H), !, heads3(T, R, 0).
heads3([H|T], R, 0) :- atomic(H), !, heads3(T, R, 0).
heads3([H|T], R, _) :- 
    heads3(H, R1, 1), heads3(T, R2, 0), append(R1, R2, R).
heads3(L, R) :- heads3(L, R, 1).

member1(H, [H|_]) :- !.
member1(X, [H|_]) :- member1(X, H).
member1(X, [_|T]) :- member1(X, T).

noAtomicElements([], 0).
noAtomicElements([H|T], R) :- 
    atomic(H), !, noAtomicElements(T, R1), R is R1 + 1.
noAtomicElements([H|T], R) :- 
    noAtomicElements(H, R1), noAtomicElements(T, R2), R is R1 + R2.

getLastAtomic([], []).
getLastAtomic([H], [H]) :- atomic(H), !.
getLastAtomic([H|T], R) :- atomic(H), !, getLastAtomic(T, R).
getLastAtomic([H|T], R) :- 
    getLastAtomic(H, R1), getLastAtomic(T, R2), append(R1, R2, R).

replace(_, _, [], []) :- !.
replace(X, NewX, [X|T], [NewX|R]) :- !, replace(X, NewX, T, R).
replace(X, NewX, [H|T], [H|R]) :- atomic(H), !, replace(X, NewX, T, R).
replace(X, NewX, [H1|T], [H2|R]) :- 
    replace(X, NewX, H1, H2), replace(X, NewX, T, R).