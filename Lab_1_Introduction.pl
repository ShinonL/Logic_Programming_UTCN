woman(ana). % Remember, predicate names are constant (start with lowercase letter)
woman(sara).
woman(ema).
woman(maria).
woman(dorina).
woman(irina).
woman(carmen).

man(andrei).
man(george).
man(alex).
man(sergiu).
man(marius).
man(mihai).

parent(maria, ana). % maria is ana’s parent
parent(george, ana). % george also is ana’s parent
parent(maria, andrei).
parent(george, andrei).
parent(marius, maria).
parent(dorina, maria).
parent(mihai, george).
parent(mihai, carmen).
parent(irina, george).
parent(irina, carmen).
parent(carmen, sara).
parent(carmen, ema).
parent(alex, sara).
parent(alex, ema).
parent(sergiu, mihai).

ancestor(X, X).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

mother(X, Y) :- woman(X), parent(X, Y).
father(X, Y) :- man(X), parent(X, Y).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
sister(X,Y) :- sibling(X, Y), woman(X).
brother(X, Y) :- sibling(X, Y), man(X).
aunt(X,Y) :- sister(X, Z), parent(Z, Y).
uncle(X,Y) :- brother(X, Z), parent(Z, Y).
grandmother(X, Y) :- mother(X, Z), mother(Z, Y).
grandfather(X, Y) :- father(X, Z), father(Z, Y).
