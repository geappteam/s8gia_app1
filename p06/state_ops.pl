:- ensure_loaded(p06).

move_player(1,[Id, Name, X, Y, B], [Id, Name, X, Yout, B]) :-
    Yout is Y + 1.
move_player(2,[Id, Name, X, Y, B], [Id, Name, Xout, Y, B]) :-
    Xout is X + 1.
move_player(3,[Id, Name, X, Y, B], [Id, Name, X, Yout, B]) :-
    Yout is Y - 1.
move_player(4,[Id, Name, X, Y, B], [Id, Name, Xout, Y, B]) :-
    Xout is X - 1.
move_player(5,[Id, Name, X, Y, B], [Id, Name, Xout, Yout, B]) :-
    Yout is Y + 1,
    Xout is X + 1.
move_player(6,[Id, Name, X, Y, B], [Id, Name, Xout, Yout, B]) :-
    Yout is Y - 1,
    Xout is X + 1.
move_player(7,[Id, Name, X, Y, B], [Id, Name, Xout, Yout, B]) :-
    Yout is Y - 1,
    Xout is X - 1.
move_player(8,[Id, Name, X, Y, B], [Id, Name, Xout, Yout, B]) :-
    Yout is Y + 1,
    Xout is X - 1.

move(Direction, OriginalState, TransformedState) :-
    OriginalState = [N, M, C, R,  OriginalPlayerList, B],
    i_move(Direction, OriginalPlayerList, TransformedPlayerList),
    TransformedState = [N, M, C, R,  TransformedPlayerList, B],
    noCollisions(TransformedState).

i_move(Direction, [AiPlayer|Tail], [MovedPlayer|Tail]) :-
    p06_nom(Name),
    AiPlayer = [_, Name, _, _, _],
    move_player(Direction, AiPlayer, MovedPlayer),
    MovedPlayer = [_, _, X, Y, _],
    X >= 0, X < C,
    Y >= 0, Y < R.
i_move(Direction, [OtherPlayer|Tail], [OtherPlayer|Rest]) :-
    p06_nom(Name),
    AiPlayer \= [_, Name, _, _, _],
    i_move(Direction, Tail, Rest).
