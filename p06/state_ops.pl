:- ensure_loaded(p06).
:- use_module(library(lists)).




% Valid state operations
modify_state(move(Direction), OriginalState, TransformedState) :-
    between(1, 8, Direction),
    move(Direction, OriginalState, TransformedState).
modify_state(take(Direction), OriginalState, TransformedState) :-
    between(1, 8, Direction),
    take(Direction, OriginalState, TransformedState).
modify_state(drop(Direction), OriginalState, TransformedState) :-
    between(1, 8, Direction),
    drop(Direction, OriginalState, TransformedState).
modify_state(attack(Direction), OriginalState, TransformedState) :-
    between(1, 8, Direction),
    attack(Direction, OriginalState, TransformedState).
modify_state(none, State, State).




% Movement actions
move(Direction, OriginalState, TransformedState) :-
    OriginalState = [N, M, C, R,  OriginalPlayerList, B],
    i_move(Direction, OriginalPlayerList, TransformedPlayerList),
    noPlayerOutOfBounds(C, R, TransformedPlayerList),
    TransformedState = [N, M, C, R,  TransformedPlayerList, B],
    noCollisions(TransformedState).

i_move(Direction, [AiPlayer|Tail], [MovedPlayer|Tail]) :-
    p06_nom(Name),
    AiPlayer = [Id, Name, X, Y, Block],
    directionTransform(Direction, [X,Y], [Xout, Yout]),
    MovedPlayer = [Id, Name, Xout, Yout, Block].
i_move(Direction, [OtherPlayer|Tail], [OtherPlayer|Rest]) :-
    p06_nom(Name),
    OtherPlayer \= [_, Name, _, _, _],
    i_move(Direction, Tail, Rest).





% Taking actions
take(Direction, OriginalState, TransformedState) :-
    p06_nom(Name),
    OriginalState = [N, M, C, R, PlayerList, BlockList],
    member([_Id, Name, Xplayer, Yplayer, PlayerBlockValue], PlayerList),!,
    directionTransform(Direction, [Xplayer, Yplayer], [Xblock, Yblock]),
    i_takeBlockInList([Xblock, Yblock], PlayerBlockValue, BlockList, ValueTaken, NewBlockList),
    i_updateBlockCount(M, PlayerBlockValue, NewM),
    i_updatePlayerBlock(ValueTaken, PlayerList, NewPlayerList),
    TransformedState = [N, NewM, C, R, NewPlayerList, NewBlockList].

i_takeBlockInList([X,Y], 0, [[ValueTaken, X, Y]|BlockList], ValueTaken, BlockList) :- !.
i_takeBlockInList([X,Y], ReplacementValue, [[ValueTaken, X, Y]|BlockList], ValueTaken, [[ReplacementValue, X, Y]|BlockList]) :- !.
i_takeBlockInList([X,Y], ReplacementValue, [NoMatch|BlockListTail], ValueTaken, [NoMatch|NewBlockListTail]) :-
    i_takeBlockInList([X,Y], ReplacementValue, BlockListTail, ValueTaken, NewBlockListTail).

i_updateBlockCount(M, 0, NewM) :-
    !, NewM is M - 1.
i_updateBlockCount(M, _, M).

i_updatePlayerBlock(NewBlock, [[Id, Name, X, Y, _B]|PlayerList], [[Id, Name, X, Y, NewBlock]|PlayerList]) :-
    p06_nom(Name),!.
i_updatePlayerBlock(NewBlock, [NoMatchPlayer|PlayerListTail], [NoMatchPlayer|TransformedPlayerList]) :-
    i_updatePlayerBlock(NewBlock, PlayerListTail, TransformedPlayerList).





% Drop action
drop(Direction, OriginalState, TransformedState) :-
    p06_nom(Name),
    OriginalState = [N, M, C, R, PlayerList, BlockList],
    member([_Id, Name, Xplayer, Yplayer, PlayerBlockValue], PlayerList),!,
    PlayerBlockValue > 0,
    NewM is M + 1,
    directionTransform(Direction, [Xplayer, Yplayer], [Xblock, Yblock]),
    positionInbound([C, R], [Xblock, Yblock]),
    NewBlockList = [[PlayerBlockValue, Xblock, Yblock]|BlockList],
    i_updatePlayerBlock(0, PlayerList, NewPlayerList),
    TransformedState = [N, NewM, C, R, NewPlayerList, NewBlockList],
    noCollisions(TransformedState).






% Attack action
attack(Direction, OriginalState, TransformedState) :-
    p06_nom(Name),
    OriginalState = [N, M, C, R, PlayerList, B],
    member([_Id, Name, Xplayer, Yplayer, PlayerBlockValue], PlayerList),!,
    directionTransform(Direction, [Xplayer, Yplayer], [Xtarget, Ytarget]),
    member([TargetId, _Name, Xtarget, Ytarget, TargetBlockValue], PlayerList),!,
    TargetBlockValue > 0,
    i_attackEsp(PlayerBlockValue, TargetBlockValue, AttackerEsp, DefenderEsp),
    i_updatePlayerBlock(AttackerEsp, PlayerList, Pl1),
    i_updatePlayerBlockById(TargetId, DefenderEsp, Pl1, NewPlayerList),
    TransformedState = [N, M, C, R, NewPlayerList, B].

i_attackEsp(0, DefenderValue, AttackerEsp, DefenderEsp) :-
    !,
    i_valueEsp(0.25, DefenderValue, 0, AttackerEsp),
    i_valueEsp(0.25, 0, DefenderValue, DefenderEsp).
i_attackEsp(AttackerValue, DefenderValue, AttackEsp, DefendEsp) :-
    SwapProb is AttackerValue / (AttackerValue + DefenderValue),
    i_valueEsp(SwapProb, DefenderValue, AttackerValue, AttackEsp),
    i_valueEsp(SwapProb, AttackerValue, DefenderValue, DefendEsp).

i_valueEsp(ProbOfFirst, First, Second, Esp) :-
    Esp is ProbOfFirst * First + (1 - ProbOfFirst) * Second.

i_updatePlayerBlockById(TargetId, BlockValue, [[TargetId, Name, X, Y, _Block]|BlockListTail], [[TargetId, Name, X, Y, BlockValue]|BlockListTail]) :- !.
i_updatePlayerBlockById(TargetId, BlockValue, [NoMatch|BlockListTail], [NoMatch|NewBlockListTail]) :-
    i_updatePlayerBlockById(TargetId, BlockValue, BlockListTail, NewBlockListTail).






% Handling direction transformations
directionTransform(1, [X, Y], [X, Yout]) :- Yout is Y + 1.
directionTransform(2, [X, Y], [Xout, Y]) :- Xout is X + 1.
directionTransform(3, [X, Y], [X, Yout]) :- Yout is Y - 1.
directionTransform(4, [X, Y], [Xout, Y]) :- Xout is X - 1.
directionTransform(5, [X, Y], [Xout, Yout]) :- Xout is X + 1, Yout is Y + 1.
directionTransform(6, [X, Y], [Xout, Yout]) :- Xout is X + 1, Yout is Y - 1.
directionTransform(7, [X, Y], [Xout, Yout]) :- Xout is X - 1, Yout is Y - 1.
directionTransform(8, [X, Y], [Xout, Yout]) :- Xout is X - 1, Yout is Y + 1.





% Collision Detection to ensure validity of move
noCollisions([_N, _M, _C, _R, PlayerList, BlockList]) :-
    empty_set(EmptySet),
    addPlayersToSet(PlayerList, EmptySet, PlayerSet),
    addBlocksToSet(BlockList, PlayerSet, _).

addBlocksToSet([], Set, Set).
addBlocksToSet([[_Id, X, Y]|BlockListTail], OccupiedSet, OutputSet) :-
    \+ member([X,Y], OccupiedSet),
    addBlocksToSet(BlockListTail, [[X,Y]|OccupiedSet], OutputSet).

addPlayersToSet([], Set, Set).
addPlayersToSet([[_Id, _Nom, X, Y, _B]|PlayerListTail], OccupiedSet, OutputSet) :-
    \+ member([X,Y], OccupiedSet),
    addPlayersToSet(PlayerListTail, [[X,Y]|OccupiedSet], OutputSet).





% Check for objects out of bounds
noObjectsOutOfBounds([_N, _M, Columns, Rows, PlayerList, BlockList]) :-
    noPlayerOutOfBounds(Columns, Rows, PlayerList),
    noBlockOutOfBounds(Columns, Rows, BlockList).

noPlayerOutOfBounds(_Columns, _Rows, []).
noPlayerOutOfBounds(Columns, Rows, [[_Id, _Nom, X, Y, _B]|PlayerListTail]) :-
    positionInbound([Columns, Rows], [X, Y]),
    noPlayerOutOfBounds(Columns, Rows, PlayerListTail).

noBlockOutOfBounds(_Columns, _Rows, []).
noBlockOutOfBounds(Columns, Rows, [[_Id, X, Y]|BlockListTail]) :-
    positionInbound([Columns, Rows], [X, Y]),
    noBlockOutOfBounds(Columns, Rows, BlockListTail).

positionInbound([C, R], [X, Y]) :-
    X >= 0, X < C,
    Y >= 0, Y < R.
