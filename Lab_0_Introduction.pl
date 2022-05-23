eats(fred, mango).

eats(human, fruits).
eats(human, meat).
eats(human, nuts).

human(socrates).
mortal(X) :- human(X).

hold_party(X) :- birthday(X), happy(X).
birthday(maria).
birthday(andrei).
birthday(cosmin).

happy(ion).
happy(cristi).
happy(andrei).

on_route(camin).
on_route(Place) :- move(Place, Method, NewPlace), on_route(NewPlace).
move(acasa, taxi, gara).
move(gara, tren, cluj).
move(cluj, bus, camin).