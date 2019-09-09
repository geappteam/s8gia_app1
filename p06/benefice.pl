
benefice(Action, BoardState, Benefice) :-
    modify_state(Action, BoardState, NewBoardState),
    stateEval(NewBoardState, Benefice).



stateEval(BoardState, Total) :-
    heldBlockValue(BoardState, BlockValue),
    potentialGain(BoardState, PotentialGain),
    threatLevel(BoardState, ThreatLevel),
    Kb is 1, Kp is 1, Kt is 1,
    Total is Kb * BlockValue + Kp * PotentialGain + Kt * ThreatLevel.
