add(X, LS, LE, RS, RE) :- RS = LS, LE = [X|RE].

inorder(nil, L, L).
inorder(t(K, L, R), LS, LE) :- 
    inorder(L, LLS, LLE), inorder(R, RLS, RLE), 
    LS = LLS, LLE = [K|RLS], LE = RLE.

quicksort([H|T], S, E) :- 
    partition(H, T, Smaller, Larger),
    quicksort(Smaller, S, [H|L]),
    quicksort(Larger, L, E),
quicksort([], L, L).

partition(P, [X|T], [X|Smaller], Larger) :- X < P, !, partition(P, T, Smaller, Larger).
partition(P, [X|T], Smaller, [X|Larger]) :- partition(P, T, Smaller, Larger).
partition(_, [], [], []).

all_perms(L, _) :- perm(L, L1), assert(p(L1)), fail.
all_perms(_, R) :- collect_perms(R).

collect_perms([L1|R]) :- retract(p(L1)), !, collect_perms(R).
collect_perms([]).

fib(N, F) :- memo_fib(N, F), !.
fib(N, F) :- N > 1, N1 is N-1, N2 is N-2,
    fib(N1, F1), fib(N2, F2),
    F is F1+F2, assert(memo_fib(N, F)).
fib(0, 1).
fib(1, 1).

print_all :- 
    memo_fib(N, F), write(N), write(‘ - ‘), write(F), nl, fail.
print_all.

convert_i_to_diff(L, RE, RE) :- var(L), !.
convert_i_to_diff([H|T], [H|RS], RE) :- convert_to_diff(T, RS, RE).		

convert_d_to_incom(LS, LE, _) :- LS == LE, !.
convert_d_to_incom([H|TLS], LE, [H|R]) :- convert_to_incom(TLS, LE, R).

convert_c_to_diff([], RE, RE) :- !.
convert_c_to_diff([H|T], [H|RS], RE) :- convert_c_to_diff(T, RS, RE).

convert_d_to_compl(LS, LE, []) :- LS == LE, !.
convert_d_to_compl([H|TLS], LE, [H|R]) :- convert_d_to_compl(TLS, LE, R).

all_decompositions(L, _) :- 
    append(L1, L2, L), assert(p([L1, L2])), fail.
all_decompositions(_, R) :- collect_perms(R).

flatten([], L, L).
flatten([H|T], [H|LS], LE) :- atomic(H), !, flatten(T, LS, LE).
flatten([H|T], LS, LE) :- flatten(H, LS, LT), flatten(T, LT, LE).

tree2(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).

collect_even(nil, L, L).
collect_even(t(K, L, R), LS, LE) :- 
    0 is (K mod 2), collect_even(L, LS, [K|LT]), collect_even(R, LT, LE).
collect_even(t(_, L, R), LS, LE) :-
    collect_even(L, LS, LT), collect_even(R, LT, LE).

tree_i(t(6, t(4, t(2, _, nil), t(5, _, _)), t(9, t(7, _, _), _))).

collect_between(T, _, _, L, L) :- var(T), !.
collect_between(nil, _, _, L, L).
collect_between(t(K, L, R), K1, K2, LS, LE) :- 
    K1 < K, K < K2, 
    collect_between(L, K1, K2, LS, [K|LT]), 
    collect_between(R, K1, K2, LT, LE).
collect_between(t(_, L, R), K1, K2, LS, LE) :-
    collect_between(L, K1, K2, LS, LT), 
    collect_between(R, K1, K2, LT, LE).