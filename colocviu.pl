% Subject: 13


% Ex 3
number_to_list(0, []).
number_to_list(X, R) :- 
    Digit is X mod 10,
    Rem is X // 10,
    number_to_list(Rem, R1), insert_ord(Digit, R1, R), !.
insert_ord(X, [], [X]).
insert_ord(X, [H|T], [H|R]) :- X > H, insert_ord(X, T, R).
insert_ord(X, L, [X|L]).


% Ex 4
tree2(t(6,t(3,t(2,_,_),_),t(9,t(7,_,_),_))).

desc_order(T, _) :- var(T), !.
desc_order(t(K, L, R), t(FRL, LRL, RRL)) :- 
    in_order(L, R1), in_order(R, R2), 
    append([K|R1], R2, RL), in_sort(RL, FRL), 
    desc_order(L, LRL), desc_order(R, RRL).

in_order(T, []) :- var(T), !.
in_order(t(K, L, R), Res) :-
    in_order(L, R1),
    in_order(R, R2),
    append([K|R1], R2, Res).
    
in_sort([], []).
in_sort([H|T], R) :- in_sort(T, R1), insert_ord(H, R1, R).

           
 

    