:- ensure_loaded(p06).
:- ensure_loaded(actions).
:- use_module(library(lists)).





% Simple Best First Heuristic
blockBlitzHeuristic(BoardState, Value) :-
    stateEval(BoardState, Quality),
    Value is 1 / Quality.

% Simple Goal Condition
blockBlitzGoal(BoardState) :-
    heldBlockValue(BoardState, MaxBlock),
    maxBlockOnField(BoardState, [MaxBlock, _BlockPosition]),
    playerAtLowestThreat(BoardState).

playerAtLowestThreat(BoardState) :-
    BoardState = [_N, _M, C, R, _P, _B],
    threatLevel(BoardState, MinThreat),
    Mci is C - 1, Mri is R - 1,
    between(0, Mci, X),
    between(0, Mri, Y),
    findall(Threat, threatLevelAt(BoardState, [X, Y], Threat), ThreatList),
    min_list(ThreatList, MinThreat).





% Specific value of action
benefice(Action, BoardState, Benefice) :-
    modify_state(Action, BoardState, NewBoardState),
    stateEval(NewBoardState, Benefice).
benefice(Action, BoardState, 0) :-
    \+ modify_state(Action, BoardState, _).




% Evaluate quantitatively the state of the board
stateEval(BoardState, Total) :-
    blockReward(BoardState, BlockReward),
    potentialGain(BoardState, PotentialGain),
    threatLevel(BoardState, ThreatLevel),
    Kb is 1, Kp is 1, Kt is 1,
    Total is Kb * BlockReward + Kp * PotentialGain - Kt * ThreatLevel.





% Block Reward Heuristic
blockReward(BoardState, BlockReward) :-
    heldBlockValue(BoardState, BlockValue),
    BlockReward is BlockValue ** 2.





% Enemy Threat heuristic
threatLevel(BoardState, Threat) :-
    playerPosition(BoardState, PlayerPos),
    threatLevelAt(BoardState, PlayerPos, Threat).

threatLevelAt(BoardState, _Position, 0) :-
    heldBlockValue(BoardState, 0).
threatLevelAt(BoardState, EvaluatedPosition, Threat) :-
    heldBlockValue(BoardState, Preciousness),
    BoardState = [_N, _M, _C, _R, PlayerList, _B],
    addThreats(PlayerList, Preciousness, EvaluatedPosition, Threat).

addThreats([], _Preciousness, _EvaluatedPosition, 0).
addThreats([[_Id, Name, X, Y, BlockValue]|PlayerListTail], Preciousness, EvaluatedPosition, TotalThreat) :-
    \+ p06_nom(Name),
    BlockValue < Preciousness,!,
    distance([X, Y], EvaluatedPosition, Distance),
    pThreat(BlockValue, Preciousness, ProbT),
    Threat is ProbT / (Distance ** 2),
    addThreats(PlayerListTail, Preciousness, EvaluatedPosition, OtherThreats),
    TotalThreat is Threat + OtherThreats.
addThreats([_NotThreat|PlayerListTail], Preciousness, EvaluatedPosition, Threat) :-
    addThreats(PlayerListTail, Preciousness, EvaluatedPosition, Threat).

pThreat(0, _DefenderValue, 0.25) :- !.
pThreat(AttackerValue, DefenderValue, ProbEx) :-
    ProbEx is AttackerValue / (AttackerValue + DefenderValue).





% Compute Proximity to Winning Block Heuristic
potentialGain(BoardState, BlockReward) :-
    playerPosition(BoardState, PlayerPosition),
    playerGoalGradient(BoardState, PlayerPosition, BlockReward).

playerGoalGradient(BoardState, _Position, BlockReward) :-
    heldBlockValue(BoardState, BlockReward),
    maxBlockOnField(BoardState, [BlockReward, _BlockPos]), !.
playerGoalGradient(BoardState, EvaluatedPosition, BlockReward) :-
    maxBlockOnField(BoardState, [BlockValue, BlockPosition]),
    distance(BlockPosition, EvaluatedPosition, Distance),
    BlockReward is BlockValue / Distance.






% Information about the game
heldBlockValue([_N, _M, _C, _R, PlayerList, _B], PlayerBlockValue) :-
    p06_nom(Name),
    member([_Id, Name, _X, _Y, PlayerBlockValue], PlayerList),!.

playerPosition([_N, _M, _C, _R, PlayerList, _B], [X, Y]) :-
    p06_nom(Name),
    member([_Id, Name, X, Y, _BV], PlayerList),!.

distance([X1, Y1], [X2, Y2], Dx) :-
    Dx is abs(X2 - X1),
    Dy is abs(Y2 - Y1),
    Dx > Dy, !.
distance([_X1, Y1], [_X2, Y2], Dy) :-
    Dy is abs(Y2 - Y1).





% Pure block model : [BlockValue, [X, Y]]
maxBlockOnField([_N, _M, _C, _R, PlayerList, BlockList], MaxBlock) :-
    maxPlayerBlock(PlayerList, MaxPlayerBlock),
    maxLayingBlock(BlockList, MaxLayingBlock),
    greaterBlock(MaxLayingBlock, MaxPlayerBlock, MaxBlock).

maxPlayerBlock([[_Id, _Name, X, Y, B]|[]], [B, [X,Y]]) :- !.
maxPlayerBlock([[_Id, _Name, X, Y, B]|PlayerListTail], MaxBlock) :-
    maxPlayerBlock(PlayerListTail, SubsequentMaxBlock),
    greaterBlock([B, [X,Y]], SubsequentMaxBlock, MaxBlock).

maxLayingBlock([[B, X, Y]|[]], [B, [X,Y]]) :- !.
maxLayingBlock([[B, X, Y]|BlockListTail], MaxBlock) :-
    maxLayingBlock(BlockListTail, SubsequentMaxBlock),
    greaterBlock([B, [X,Y]], SubsequentMaxBlock, MaxBlock).

greaterBlock([BlockValue1, _BlockPos1], [BlockValue2, BlockPos2], [BlockValue2, BlockPos2]) :-
    BlockValue2 > BlockValue1, !.
greaterBlock(Block1, _Block2, Block1).
