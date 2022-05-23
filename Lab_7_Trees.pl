tree2(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).
       
inorder(t(K, L, R), List) :- 
    inorder(L, LL), inorder(R, LR), append(LL, [K|LR], List). 
inorder(nil, []). 

preorder(t(K, L, R), List) :-
    preorder(L, LL), preorder(R, LR), append([K|LL], LR, List). 
preorder(nil, []). 

postorder(t(K, L, R), List) :-
    postorder(L, LL), postorder(R, LR),
    append(LL, LR, R1), append(R1, [K], List). 
postorder(nil, []).

pretty_print(nil, _).
pretty_print(t(K, L, R), D):-
    D1 is D + 1, 
    pretty_print(L, D1), 
	print_key(K, D), 
    pretty_print(R, D1).

print_key(K, D) :- D > 0, !, D1 is D - 1, tab(D), print_key(K, D1). 
print_key(K, _) :- write(K), nl. 

search_key(Key, t(Key, _, _)) :- !. 
search_key(Key, t(K, L, _)) :- Key < K, !, search_key(Key, L). 
search_key(Key, t(_, _, R)) :- search_key(Key, R). 

insert_key(Key, nil, t(Key, nil, nil)) :- write('Inserted '), write(Key), nl. 
insert_key(Key, t(Key, L, R), t(Key, L, R)) :- !, write('Key already in tree\n'). 
insert_key(Key, t(K, L, R), t(K, NL, R)) :- Key < K, !, insert_key(Key, L, NL). 
insert_key(Key, t(K, L, R), t(K, L, NR)) :- insert_key(Key, R, NR). 

delete_key(Key, nil, nil) :- write(Key), write(' not in tree'), nl.
delete_key(Key, t(Key, L, nil), L) :- !. 
delete_key(Key, t(Key, nil, R), R) :- !. 
delete_key(Key, t(Key, L, R), t(Pred, NL, R)) :- !, get_pred(L, Pred, NL). 
delete_key(Key, t(K, L, R), t(K, NL, R)) :- Key < K, !, delete_key(Key, L, NL). 
delete_key(Key, t(K, L, R), t(K, L, NR)) :- delete_key(Key, R, NR). 

get_pred(t(Pred, L, nil), Pred, L) :- !. 
get_pred(t(Key, L, R), Pred, t(Key, L, NR)) :- get_pred(R, Pred, NR). 
 
max(A, B, A) :- A > B, !. 
max(_, B, B). 

height(nil, 0). 
height(t(_, L, R), H) :- 
    height(L, H1), height(R, H2), max(H1, H2, H3), H is H3 + 1. 

tree3(t(6, t(4, t(2, nil, nil, nil), t(7, nil, nil, nil), nil), t(5, nil, nil, nil), t(9, t(3, nil, nil, nil), nil, nil))).

pretty_print3(nil, _).
pretty_print3(t(K, L, M, R), D) :-
    D1 is D + 1, 
    pretty_print3(L, D1), 
	print_key3(K, D), 
	pretty_print3(M, D1),
	pretty_print3(R, D1).
						
print_key3(K, D) :- D > 0, !, D1 is D - 1, tab(D), print_key3(K, D1).
print_key3(K, _) :- write(K), nl.

inorder3(t(K, L, M, R), List) :-
    inorder3(L, LL), inorder3(M, LM), inorder3(R, LR),
	append(LL, [K|LM], Rez), append(Rez, LR, List).
inorder3(nil, []).

preorder3(t(K, L, M, R), List) :-
	preorder3(L, LL), preorder3(M, LM), preorder3(R, LR),
	append([K|LL], LM, Rez), append(Rez, LR, List).
preorder3(nil, []).

postorder3(t(K, L, M, R), List) :-
	postorder3(L, LL), postorder3(M, LM), postorder3(R, LR),
	append(LL, LM, R1), append(R1, LR, R2), append(R2, [K], List).
postorder3(nil, []).

height3(nil, 0).
height3(t(_, L, M, R), H) :-
	height3(L, H1), height3(M, H2), height3(R, H3), 
	max(H1, H2, Max1), max(Max1, H3, Max2),
	H is Max2 + 1.

inorder2_print(t(K, L, R)) :-
	inorder2_print(L), inorder2_print(R), write(K), write('\s').
inorder2_print(nil).

delete_hang(Key, nil, nil) :- write(Key), write(' not in tree\n'). 
delete_hang(Key, t(Key, L, nil), L) :- !. 
delete_hang(Key, t(Key, nil, R), R) :- !. 
delete_hang(Key, t(Key, L, R), Rez) :- !, hang(L, R, Rez). 
delete_hang(Key, t(K, L, R), t(K, NL, R)) :- Key < K, !, delete_hang(Key, L, NL). 
delete_hang(Key, t(K, L, R), t(K, L, NR)) :- delete_hang(Key, R, NR). 
											
hang(nil, R, R) :- !.
hang(T, nil, T) :- !.
hang(T, t(K, nil, R), t(K, T, R)) :- !.
hang(T, t(K, L, R), t(K, NL, R)) :- hang(T, L, NL).

leafs(nil, []).
leafs(t(K, nil, nil), [K]) :- !.
leafs(t(_, L, R), List) :- leafs(L, L1), leafs(R, L2), append(L1, L2, List).

diameter(nil, 0).
diameter(t(_, L, R), D) :- 
    diameter(L, DL), diameter(R, DR),
	height(L, HL), height(R, HR), 
	H is HL + HR + 1, 
	max(DL, DR, D1), max(D1, H, D).

search_depth3(nil, _, []).
search_depth3(t(K, _, _, _), 0, [K]) :- !.
search_depth3(t(_, L, M, R), D, List) :-
    D > 0, D1 is D - 1,
	search_depth3(L, D1, LL), search_depth3(M, D1, LM), search_depth3(R, D1, LR),
	append(LL, LM, L1), append(L1, LR, List).

sym_tree(t(6, t(4, nil, t(5, nil, nil)), t(9, t(7, nil, nil), nil))).

symmetric(nil).
symmetric(t(_, L, R)) :- check_mirror(L, R).

check_mirror(nil, nil).
check_mirror(t(_, L1, R1), t(_, L2, R2)) :- 
    check_mirror(L1, R2), check_mirror(R1, L2).

