member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

delete(X, [X|T], T).
delete(X, [H|T], [H|R]) :- delete(X, T, R).
delete(_, [], []).

delete_all(X, [X|T], R) :- delete_all(X, T, R).
delete_all(X, [H|T], [H|R]) :- delete_all(X, T, R).
delete_all(_, [], []).

append3_simple(L1, L2, L3, R) :- append(L1, L2, Res), append(Res, L3, R).

append3([], [], L3, L3).
append3([], [H2|T2], L3, [H2|R]) :- append3([], T2, L3, R).
append3([H1|T1], L2, L3, [H1|R]) :- append3(T1, L2, L3, R).

insert(X, L, [X|L]).

sum([], 0).
sum([H|T], R) :- sum(T, Res), R is H + Res.

separate_parity([], [], []).
separate_parity([H|T], [H|E], O) :- 0 is (H mod 2), separate_parity(T, E, O).
separate_parity([H|T], E, [H|O]) :- 1 is (H mod 2), separate_parity(T, E, O).

remove_duplicates([], []).
remove_duplicates([H|T], R) :- remove_duplicates(T, R), member(H, R).
remove_duplicates([H|T], [H|R]) :- remove_duplicates(T, R), \+ member(H, R).

replace_all(_, _, [], []).
replace_all(K, NewK, [H|T], [NewK|R]) :- replace_all(K, NewK, T, R), H = K.
replace_all(K, NewK, [H|T], [H|R]) :- replace_all(K, NewK, T, R), H \= K.

drop(0, [_|T], T).
drop(K, [H|T], [H|R]) :- NextK is K - 1, drop(NextK, T, R).