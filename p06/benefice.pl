:- ensure_loaded(p06).
:- ensure_loaded(state_ops).




% Specific value of action
benefice(Action, BoardState, Benefice) :-
    modify_state(Action, BoardState, NewBoardState),
    stateEval(NewBoardState, Benefice).
benefice(Action, BoardState, 0) :-
    \+ modify_state(Action, BoardState, _).




% Evaluate quantitatively the state of the board
stateEval(BoardState, Total) :-
    heldBlockValue(BoardState, BlockValue),
    potentialGain(BoardState, PotentialGain),
    threat_level(BoardState, ThreatLevel),
    Kb is 1, Kp is 1, Kt is 1,
    Total is Kb * BlockValue + Kp * PotentialGain + Kt * ThreatLevel.





% Enemy Threat heuristic
threat_level(_BoardState, 0) :-
    % Enemies don't any interest toward the AI's possession
    heldBlockValue(_BoardState, 0).

threat_level(BoardState, ThreatLevel) :-
    % Check if enemies would have an interest toward the AI concerning its current possesion.
    heldBlockValue(BoardState, BlockValue),
    BlockValue > 0,

    % Check BoardState if there're enemies not holding blocks or holding blocks with lesser values than the AI.
    count_threats(BoardState, ListThreats, NumberThreats),
    NumberThreats > 0,

    % Sum and ouput the threat level of the AI's position according to a specific board state.
    sum_individual_threats(ListThreats, ThreatLevel).

%TODO
%count_threats()

%TODO
%sum_individual_threats()





% Information about the game
heldBlockValue([_N, _M, _C, _R, PlayerList, _B], PlayerBlockValue) :-
    p06_nom(Name),
    member([_Id, Name, _X, _Y, PlayerBlockValue], PlayerList),!.

playerPosition([_N, _M, _C, _R, PlayerList, _B], [X, Y]) :-
    p06_nom(Name),
    member([_Id, Name, X, Y, _BV], PlayerList),!.





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
    maxLayingBlock(BlockListTail, SubsequentMaxBloc),
    greaterBlock([B, [X,Y]], SubsequentMaxBlock, MaxBlock).

greaterBlock([BlockValue1, _BlockPos1], [BlockValue2, BlockPos2], [BlockValue2, BlockPos2]) :-
    BlockValue2 > BlockValue1, !.
greaterBlock(Block1, _Block2, Block1).
