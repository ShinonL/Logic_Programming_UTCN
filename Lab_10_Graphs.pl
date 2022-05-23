edge(a, b).
edge(b, a).
edge(a, d).
edge(d, a).
edge(b, d).
edge(d, b).
edge(b, c).
edge(c, b).
edge(d, c).
edge(c, d).
edge(g, e).
edge(e, g).
edge(e, f).
edge(f, e).

is_edge(X, Y) :- edge(X, Y), edge(Y, X).

%neighbor(a, [b, d]).
%neighbor(b, [a, c, d]).
%neighbor(c, [b, d]).

%Graph_1 = [
%        n(aa, [bb,dd]), 
%        n(bb, [aa,cc,dd]), 
%        n(cc, [bb,dd]), 
%        n(dd, [aa,bb,cc]), 
%        n(ee, [ff,gg]), 
%        n(ff, [ee]), 
%        n(gg, [ee]), 
%        n(hh, [])
%        ].

%Graph_2 = graph([aaa,bbb,ccc,ddd,eee,fff,ggg,hhh], 
%                [e(aaa,bbb), e(bbb,aaa), … ]).

neighbor_to_edge :- neighbor(Node, List), process(Node, List), fail.
neighbor_to_edge.
process(Node, [H|T]) :- assertz(edge(Node, H)), process(Node, T).
process(_, []).

% path(Source, Target, Path)
path(X, Y, Path) :- path(X, Y, [X], Path).
path(X, Y, PPath, FPath) :- 
    is_edge(X, Z), \+(member(Z, PPath)), path(Z, Y, [Z|PPath], FPath).
path(X, X, PPath, PPath).

% restricted_path(Source, Target, RestrictionsList, Path)
% check_restrictions(RestrictionsList, Path)
restricted_path(X, Y, LR, P) :- path(X, Y, P), check_restrictions(LR, P).
check_restrictions([], _) :- !.
check_restrictions([H|T], [H|R]) :- !, check_restrictions(T, R).
check_restrictions(T, [_|L]) :- check_restrictions(T, L).

%optimal_path(Source, Target, Path) 
:- dynamic sol_part/2.
optimal_path(X, Y, _) :- asserta(sol_part([], 100)), path_2(X, Y, [X], 1).
optimal_path(_, _, Path) :- retract(sol_part(Path, _)).

path_2(X, X, Path, LPath) :- 
    retract(sol_part(_, _)), !, asserta(sol_part(Path, LPath)), fail.
path_2(X, Y, PPath, LPath) :- 
    is_edge(X, Z), \+(member(Z, PPath)), LPath1 is LPath + 1,
	sol_part(_, Lopt), LPath1 < Lopt, path(Z, Y, [Z|PPath], LPath1).

%hamilton(NbNodes, Source, Path)
hamilton(NN, X, Path) :- NN1 is NN - 1, hamilton_path(NN1, X, X, [X], Path).

edge_to_neighbor(_) :- is_edge(X, Y), assert(n(X, Y)), fail. 
edge_to_neighbor(R) :- collect(R).

collect([n(X, [Y|Res])|R]) :- retract(n(X, Y)), process_edge(X,Res), !, collect(R).
collect([]).

process_edge(X, [Y|R]) :- retract(n(X, Y)), process_edge(X, R).
process_edge(_, []).

restricted_path_2(Dst, Src, [Dst|T], P) :- restricted_path_2(Dst, Src, T, [Dst], P).
restricted_path_2(Dst, Src, L, P) :- restricted_path_2(Dst, Src, L, [Dst], P).
restricted_path_2(Dst, Src, [H|T], PPath, FPath) :- 
    is_edge(Dst, Interm), \+(member(Interm, PPath)), make_list(Interm, H, [H|T], L), 
    restricted_path_2(Interm, Src, L, [Interm|PPath], FPath).
restricted_path_2(Dst, Src, RL, PPath, FPath) :- 
    is_edge(Dst, Interm), \+(member(Interm, PPath)), restricted_path_2(Interm, Src, RL, [Interm|PPath], FPath).
restricted_path_2(Dst, Dst, [], PPath, PPath).

make_list(V, V, [_|L], L) :- !.
make_list(_, _, L, L).

edge(a, b, 1).
edge(b, a, 2).
edge(a, d, 13).
edge(d, a, 1).
edge(b, d, 3).
edge(d, b, 4).
edge(b, c, 12).
edge(c, b, 33).
edge(d, c, 12).
edge(c, d, 6).
edge(g, e, 8).
edge(e, g, 45).
edge(e, f, 1).
edge(f, e, 5).

is_edge(X, Y, Cost) :- edge(X, Y, Cost); edge(Y, X, Cost).
 
optimal_cost_2(Dst, Src, _) :- asserta(sol_part([], 100)), o_path_2(Dst, Src, [Dst], 0).
optimal_cost_2(_, _, Path) :- retract(sol_part(Path, _)).

o_path_2(X, X, Path, Cost) :- retract(sol_part(_, _)), !, asserta(sol_part(Path, Cost)), fail.
o_path_2(X, Y, PPath, Cost) :- 
    is_edge(X, Z, Edge_Cost), \+(member(Z, PPath)), New_Cost is Cost + Edge_Cost,
	sol_part(_, Optim), New_Cost < Optim, o_path_2(Z, Y, [Z|PPath], New_Cost).

cycle(X, Path) :- cycle(X, X, Path).

cycle(X, Y, Path) :- cycle(X, Y, [], Path).
cycle(X, Y, PPath, FPath) :- is_edge(X, Z), \+member(Z, PPath), cycle(Z, Y, [Z|PPath], FPath).
cycle(X, X, PPath, PPath1) :- PPath \= [], append(PPath, [X], PPath1).
