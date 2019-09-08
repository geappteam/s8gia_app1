
benefice(move(Direction), _BoardState, Benefice) :-
    Direction >= 1, Direction =< 8,
    random(0,100, Benefice).

benefice(attack(Direction), _BoardState, Benefice) :-
    Direction >= 1, Direction =< 8,
    random(0,100, Benefice).

benefice(drop(Direction), _BoardState, Benefice) :-
    Direction >= 1, Direction =< 8,
    random(0,100, Benefice).

benefice(take(Direction), _BoardState, Benefice) :-
    Direction >= 1, Direction =< 8,
    random(0,100, Benefice).

benefice(none, _BoardState, Benefice) :-
    random(0,100, Benefice).
