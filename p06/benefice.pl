
benefice(Action, BoardState, Benefice) :-
    modify_state(Action, BoardState, NewBoardState),
    stateEval(NewBoardState, Benefice).

stateEval(BoardState, Total) :-
    heldBlockValue(BoardState, BlockValue),
    potentialGain(BoardState, PotentialGain),
    threat_level(BoardState, ThreatLevel),
    Kb is 1, Kp is 1, Kt is 1,
    Total is Kb * BlockValue + Kp * PotentialGain + Kt * ThreatLevel.

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
