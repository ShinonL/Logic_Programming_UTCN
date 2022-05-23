% Permutation sort
perm_sort(L, R) :- perm(L, R), is_ordered(R), !.

perm(L, [H|R]) :- append(A, [H|T], L), append(A, T, L1), perm(L1, R).
perm([], []).

is_ordered([_]).
is_ordered([H1, H2|T]) :- H1 =< H2, is_ordered([H2|T]).

perm_sort_2(L, R) :- perm_2(L, R), is_ordered(R), !.

perm_2(L, [H|R]) :- delete_new(H, L, R1), perm_2(R1, R).
perm_2([], []).

delete_new(X, [X|T], T).
delete_new(X, [H|T], [H|R]) :- delete_new(X, T, R).

% Selection sort
sel_sort(L, [M|R]) :- minimum_bwd(L, M), delete(M, L, L1), sel_sort(L1, R).
sel_sort([], []).

minimum_bwd([H], H).
minimum_bwd([H|T], Min) :- minimum_bwd(T, Min), H >= Min, !.
minimum_bwd([H|_], H).

sel_sort_desc(L, [M|R]) :- maximum_bwd(L, M), delete(M, L, L1), sel_sort_desc(L1, R).
sel_sort_desc([], []).

maximum_bwd([H], H).
maximum_bwd([H|T], Max) :- maximum_bwd(T, Max), H < Max, !.
maximum_bwd([H|_], H).

% Insertion sort
delete(X, [X|T], T) :- !. 
delete(X, [H|T], [H|R]) :- delete(X, T, R). 
delete(_, [], []).

ins_sort([H|T], R) :- ins_sort(T, R1), insert_ord(H, R1, R).
ins_sort([], []).

insert_ord(X, [H|T], [H|R]) :- X > H, !, insert_ord(X, T, R).
insert_ord(X, T, [X|T]).

ins_sort_fwd(L, R) :- ins_sort_2(L, [], R).

ins_sort_2([], L, L).
ins_sort_2([H|T], L1, R) :- insert_ord(H, L1, R1), ins_sort_2(T, R1, R).

% Bubble sort
% - stops when the list is sorted
bubble_sort(L, R) :- one_pass(L, R1, F), nonvar(F), !, bubble_sort(R1, R).
bubble_sort(L, L).

one_pass([H1, H2|T], [H2|R], F) :- H1 > H2, !, F = 1, one_pass([H1|T], R, F).
one_pass([H1|T], [H1|R], F) :- one_pass(T, R, F).
one_pass([], [] ,_).

% - fixed number of passes
bubble_sort_K(L, K, R) :- 
    K > 0, K1 is K - 1, 
    one_pass(L, R1, F), nonvar(F), !, 
    bubble_sort_K(R1, K1, R).
bubble_sort_K(L, 0, L).

% Quick sort
quick_sort([H|T], R) :- 
    partition(H, T, Smaller, Larger), 
    quick_sort(Smaller, SmallerFinalResult),
	quick_sort(Larger, LargerFinalResult), 
    append(SmallerFinalResult, [H|LargerFinalResult], R).
quick_sort([], []).

partition(P, [X|T], [X|Smaller], Larger) :- X < P, !, partition(P, T, Smaller, Larger).
partition(P, [X|T], Smaller, [X|Larger]) :- partition(P, T, Smaller, Larger).
partition(_, [], [], []).

% Merge sort
merge_sort(L, R) :- 
    split(L, L1, L2), merge_sort(L1, R1), 
    merge_sort(L2, R2), merge(R1, R2, R).
merge_sort([H], [H]).
merge_sort([], []).

split(L, L1, L2) :- length(L, Len), Len > 1, K is Len / 2, splitK(L, K, L1, L2).

splitK([H|T], K, [H|L1], L2) :- K > 0, !, K1 is K - 1, splitK(T, K1, L1, L2).
splitK(T, _, [], T).

merge([H1|T1], [H2|T2], [H1|R]) :- H1 < H2, !, merge(T1, [H2|T2], R).
merge([H1|T1], [H2|T2], [H2|R]) :- merge([H1|T1], T2, R).
merge([], L, L).
merge(L, [], L).

% ASCII sort
sort_chars(L, [Min|R]) :- 
    minimum_char(L, Min), delete(Min, L, L1), sort_chars(L1, R).
sort_chars([], []).

minimum_char([H], H).
minimum_char([H|T], Min) :- 
    minimum_char(T, Min), 
    char_code(H, H_Value),
    char_code(Min, Min_Value),
    H_Value >= Min_Value, !.
minimum_char([H|_], H).

% Sort  by list length
sort_len(L, R) :- one_pass_len(L, R1, F), nonvar(F), !, sort_len(R1, R).
sort_len(L, L).

one_pass_len([H1, H2|T], [H2|R], F) :- 
    length(H1, H1_Len), length(H2, H2_Len), H1_Len > H2_Len, !, 
    F = 1, one_pass_len([H1|T], R, F).
one_pass_len([H1|T], [H1|R], F) :- one_pass_len(T, R, F).
one_pass_len([], [] ,_).