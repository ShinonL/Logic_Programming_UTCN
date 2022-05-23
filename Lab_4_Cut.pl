cls():-format('~c~s', [0x1b, "[2J"]).

member1(X, [X|_]) :- !.
member1(X, [_|T]) :- member1(X, T).

delete1(X, [X|T], T) :- !. 
delete1(X, [H|T], [H|R]) :- delete1(X, T, R). 
delete1(_, [], []).

append1([], L, L).
append1([H|T], L, [H|R]) :- append1(T, L, R).

%backward
length1([],0).
length1([_|T], Len) :- length1(T, Len1), Len is Len1 + 1.

%forward
length2([], R, R).
length2([_|T], Acc, R) :- NAcc is Acc + 1, length2(T, NAcc, R).
length2_good(L, R) :- length2(L, 0, R).

%backward
reverse1([], []).
reverse1([H|T], Res) :- reverse1(T, R1), append(R1, [H], Res).

%forward
reverse_fwd([], R, R).
reverse_fwd([H|T], Acc, R) :- reverse_fwd(T, [H|Acc], R).
reverse_fwd_pretty(L, R):- reverse_fwd(L, [], R).

%forward
minimum([], M, M).
minimum([H|T], MP, M) :- H < MP, !, minimum(T, H, M).
minimum([_|T], MP, M) :- minimum(T, MP, M).

minimum_pretty([H|T], R) :- minimum([H|T], H, R). 

%backward
minimum_bwd([H], H).
minimum_bwd([H|T], M) :- minimum_bwd(T, M), H >= M , !.
minimum_bwd([H|_], H).



union([],L,L).
union([H|T], L2, R) :- member(H,L2), !, union(T, L2, R).
union([H|T], L, [H|R]) :- union(T, L, R).

intersection([], _, []).
intersection([H|T], L, [H|R]) :- member1(H, L), !,
								intersection(T, L, R).
intersection([_|T], L, R) :- intersection(T, L, R).

difference([], _, []).
difference([H|T], L, R) :- member1(H, L), !,
							difference(T, L, R).
difference([H|T], L, [H|R]) :- difference(T, L, R).




% QUIZ EXERCISES

% Write a predicate which finds and deletes the minimum element in a list.
delete_min(List, Rez) :- minimum_pretty(List, Min),
						delete1(Min, List, Rez).
						
 
% Write a predicate which reverses the elements of a list from the Kth element
reverse_fromK(List, K, Rez) :- append1(SubList1, SubList2, List),
								length1(SubList1, Len1),
								Len1 is K - 1,
								reverse1(SubList2, Reversed),
								append1(SubList1, Reversed, Rez).
			
% Write a predicate which finds and deletes the maximum element from a list
maximum([], M, M).
maximum([H|T], MP, M) :- H > MP, !, maximum(T, H, M).
maximum([_|T], MP, M) :- maximum(T, MP, M).

maximum_pretty([H|T], M) :- maximum([H|T], H, M).

delete_max(List, Rez) :- maximum_pretty(List, Max),
						delete1(Max, List, Rez).
						
						
% PROBLEMS

% Write a predicate which performs RLE (Run-length encoding) on the elements of a list
% [element, no_occurences] 
rle_encode([], Val, Cntr, [[Val|Cntr]]).
rle_encode([H|T], H, Cntr, Rez) :- !,
									Cntr1 is Cntr + 1,
									rle_encode(T, H, Cntr1, Rez).
rle_encode([H|T], Val, Cntr, [[Val, Cntr]|Acc]) :- rle_encode(T, H, 1, Acc).

rle_encode_pretty([H|T], Rez) :- rle_encode([H|T], H, 0, Rez). 

% Write a predicate which rotates a list K positions to the right
rotate_right_Kpos(List, K, Rez) :- append1(SubList1, SubList2, List),
									length1(SubList2, K),
									append1(SubList2, SubList1, Rez).