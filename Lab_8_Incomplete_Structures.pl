member_il(_, L) :- var(L), !, fail.
member_il(X, [X|_]) :- !.
member_il(X, [_|T]) :- member_il(X, T).

insert_il(X, L) :- var(L), !, L=[X|_].
insert_il(X, [X|_]) :- !.
insert_il(X, [_|T]):- insert_il(X, T).

delete_il(_, L, L) :- var(L), !. 
delete_il(X, [X|T], T) :- !.
delete_il(X, [H|T], [H|R]) :- delete_il(X, T, R).

search_it(_, T) :- var(T), !, fail.
search_it(Key, t(Key, _, _)) :- !.
search_it(Key, t(K, L, _)) :- Key < K, !, search_it(Key, L).
search_it(Key, t(_, _, R)) :- search_it(Key, R).

insert_it(Key, t(Key, _, _)) :- !.
insert_it(Key, t(K, L, _)) :- Key < K, !, insert_it(Key, L).
insert_it(Key, t(_, _, R)):- insert_it(Key, R).

delete_it(Key, T, T) :- var(T), !, write(Key), write(' not in tree\n').
delete_it(Key, t(Key, L, R), L) :- var(R), !.
delete_it(Key, t(Key, L, R), R) :- var(L), !.
delete_it(Key, t(Key, L, R), t(Pred, NL, R)) :- 
    !, get_pred(L, Pred, NL).
delete_it(Key, t(K, L, R), t(K, NL, R)) :- 
    Key < K, !, delete_it(Key, L, NL).
delete_it(Key, t(K, L, R), t(K, L, NR)) :- delete_it(Key, R, NR).

get_pred(t(Pred, L, R), Pred, L) :- var(R), !.
get_pred(t(Key, L, R), Pred, t(Key, L, NR)) :- get_pred(R, Pred, NR).

append_il(L1, L2, L2) :- var(L1), !.
append_il([H|T], L, [H|R]) :- append_il(T, L, R), !.

reverse_il(L, R, R) :- var(L), !.
reverse_il([H|T], Acc, R) :- reverse_il(T, [H|Acc], R).
reverse_il(L, R) :- reverse_il(L, _, R).

to_complete_list(L1, []) :- var(L1), !.
to_complete_list([H|T], [H|R]) :- to_complete(T, R), !.

tree2(t(7, t(4, t(3, _, _), t(6, t(5, _, _), _)), t(11, _, _))).

preorder_it(T, _) :- var(T), !.
preorder_it(t(K, L, R), List) :- 
	preorder_it(L, LL), preorder_it(R, LR), append_il([K|LL], LR, List).

max(A, B, A) :- A > B, !. 
max(_, B, B). 

height_it(T, 0) :- var(T), !. 
height_it(t(_, L, R), H) :-
	height_it(L, HL), height_it(R, HR), max(HL, HR, HMax), H is HMax + 1.

to_complete_tree(T, nil) :- var(T), !.
to_complete_tree(t(K, L, R), t(K, LL, LR)) :- 
	to_complete_tree(L, LL), to_complete_tree(R, LR).

flatten_il(L, _) :- var(L), !.
flatten_il([H|T], [H|R]) :- atomic(H), !, flatten_il(T, R).
flatten_il([H|T], R) :- 
    flatten_il(H, R1), flatten_il(T, R2), append_il(R1, R2, R).

diameter_it(T, 0) :- var(T), !.
diameter_it(t(_, L, R), D) :- 
    diameter_it(L, DL), diameter_it(R, DR),
	height_it(L, HL), height_it(R, HR), 
	H is HL + HR + 1, 
	max(DL, DR, D1), max(D1, H, D).

sub_il(_, L):- var(L), !, fail.
sub_il([H|T1], [H|T2]) :- check(T1, T2).
sub_il(L, [_|T]) :- sub_il(L, T).

check(L, _) :- var(L), !.
check([H|T1], [H|T2]) :- check(T1, T2).


