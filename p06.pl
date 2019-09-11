:- module(p06,[p06_nom/1,p06_auteurs/1,p06_reset/0,p06_plan/1,p06_action/2]).


% Indentification
% Anthony Parris & Edouard Denommee
% Septembre 2019
p06_nom('Chuckitty').
p06_auteurs('Goku & Eddy').



% Jeux
p06_reset() :-
    setPlan([none]).

p06_plan(Plan) :-
    getPlan(Plan).

p06_action(State, Action) :-
    bestFirstPlan(State, modify_state, blockBlitzGoal, blockBlitzHeuristic, Plan),
    append(Plan, [none], [Action|CompletePlan]),
    setPlan([Action|CompletePlan]).



% Plan transfer
:- dynamic([planRestant/1]).

planRestant([none]).

getPlan(Plan) :-
    with_mutex(p00,planRestant(Plan)).

setPlan(Plan) :-
    with_mutex(p00,changePlan(Plan)).

changePlan(Plan) :-
    retractall(planRestant(_)),
    assert(planRestant(Plan)).


% Fake test plan
testPlan([4,3,4,4,[[2,'Brutus',0,2,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Chuckitty',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]]).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Best First Search algorithm
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Used in priority queue
precedes([Cost1, _State1, _Parent1], [Cost2, _Stat2, _Parent2]) :- Cost1 < Cost2.



% bestFirstPlan(+OriginalState, :SuccessorExplorer, :Goal, :Heuristic, -Plan)
% :SuccessorExplorer : (Action, OriginalState, SuccesorState)
% :Goal : (+State)
% :Heuristic : (+State, -Value)
bestFirstPlan(OriginalState, SuccessorExplorer, Goal, Heuristic, Plan) :-
    % Element in this queue : [Cost, State, Parent]
    OpenPq = [[0, OriginalState, none]],
    ClosedSet = [],
    bestFirstLoop(OpenPq, ClosedSet, SuccessorExplorer, Goal, Heuristic, StateList),
    makeActionList(SuccessorExplorer, StateList, Plan).

bestFirstLoop([[C, State, P]|_OpenSet], ClosedSet, _S, Goal, _F, StateList) :-
    call(Goal, State), !,
    buildStateList(State, [[C, State, P]|ClosedSet], InvStateList),
    reverse(InvStateList, StateList).
bestFirstLoop([Node|OpenSet], ClosedSet, SuccessorExplorer, Goal, Heuristic, StateList) :-
    Node = [_NodeCost, NodeState, _NodeParent],
    findall(S, call(SuccessorExplorer, _Action, NodeState, S), SuccessorList),
    handleSuccessors(SuccessorList, OpenSet, [Node|ClosedSet], Node, Heuristic, NewOpenSet, NewClosedSet),
    bestFirstLoop(NewOpenSet, NewClosedSet, SuccessorExplorer, Goal, Heuristic, StateList).

handleSuccessors([], OpenSet, ClosedSet, _N, _H, OpenSet, ClosedSet).
handleSuccessors([SState|SList], OpenSet, ClosedSet, Parent, Heuristic, OpenSetOut, ClosedSetOut) :-
    member([OldCost, SState, OldParent], ClosedSet),
    Parent = [ParentCost, ParentState, _ParentParent],
    call(Heuristic, SState, StateCost),
    NewCost is StateCost + ParentCost,
    OldCost > NewCost, !,
    delete(ClosedSet, [OldCost, SState, OldParent], ClosedSet1),
    insert_pq([NewCost, SState, ParentState], OpenSet, OpenSet1),
    handleSuccessors(SList, OpenSet1, ClosedSet1, Parent, Heuristic, OpenSetOut, ClosedSetOut).
handleSuccessors([SState|SList], OpenSet, ClosedSet, Parent, Heuristic, OpenSetOut, ClosedSetOut) :-
    member([OldCost, SState, OldParent], OpenSet),
    Parent = [ParentCost, ParentState, _ParentParent],
    call(Heuristic, SState, StateCost),
    NewCost is StateCost + ParentCost,
    OldCost > NewCost, !,
    delete(OpenSet, [OldCost, SState, OldParent], OpenSet1),
    insert_pq([NewCost, SState, ParentState], OpenSet1, OpenSet2),
    handleSuccessors(SList, OpenSet2, ClosedSet, Parent, Heuristic, OpenSetOut, ClosedSetOut).
handleSuccessors([SState|SList], OpenSet, ClosedSet, Parent, Heuristic, OpenSetOut, ClosedSetOut) :-
    \+ member([_, SState, _], ClosedSet),
    \+ member([_, SState, _], OpenSet),!,
    Parent = [ParentCost, ParentState, _ParentParent],
    call(Heuristic, SState, StateCost),
    NewCost is StateCost + ParentCost,
    insert_pq([NewCost, SState, ParentState], OpenSet, OpenSet1),
    handleSuccessors(SList, OpenSet1, ClosedSet, Parent, Heuristic, OpenSetOut, ClosedSetOut).
handleSuccessors([_SState|SList], OpenSet, ClosedSet, Parent, Heuristic, OpenSetOut, ClosedSetOut) :-
    handleSuccessors(SList, OpenSet, ClosedSet, Parent, Heuristic, OpenSetOut, ClosedSetOut).

buildStateList(none, _ClosedSet, []) :- !.
buildStateList(State, ClosedSet, [State|InvStateList]) :-
    member([_Cost, State, ParentState], ClosedSet),!,
    buildStateList(ParentState, ClosedSet, InvStateList).

makeActionList(_SuccessorExplorer, [_LastState], []).
makeActionList(SuccessorExplorer, [State1, State2| StateListTail], [Action|ActionList]) :-
    call(SuccessorExplorer, Action, State1, State2),
    makeActionList(SuccessorExplorer, [State2|StateListTail], ActionList).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Rules, Actions and Effects
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% List of valid actions
is_action(move(Direction)) :- between(1, 8, Direction).
is_action(attack(Direction)) :- between(1, 8, Direction).
is_action(drop(Direction)) :- between(1, 8, Direction).
is_action(take(Direction)) :- between(1, 8, Direction).
is_action(none).


% Valid state operations
modify_state(none, State, State).
modify_state(Action, OriginalState, TransformedState) :-
    is_action(Action),
    Action \= none,
    call(Action, OriginalState, TransformedState).




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
    addPlayersToSet(PlayerList, [], PlayerSet),
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
    Mci is C - 1, Mri is R - 1,
    between(0, Mci, X),
    between(0, Mri, Y).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Game Heuristics
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    threatLevel(BoardState, MinThreat),
    findall(Threat, threatLevelAt(BoardState, _Pos, Threat), ThreatList),
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
    Kb is 1, Kp is 1, Kt is 2,
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
    BoardState = [_N, _M, C, R, PlayerList, _B],
    positionInbound([C, R], EvaluatedPosition),
    addThreats(PlayerList, Preciousness, EvaluatedPosition, Threat).

addThreats([], _Preciousness, _EvaluatedPosition, 0).
addThreats([[_Id, Name, X, Y, BlockValue]|PlayerListTail], Preciousness, EvaluatedPosition, TotalThreat) :-
    \+ p06_nom(Name),
    BlockValue < Preciousness,
    distance([X, Y], EvaluatedPosition, 0),!,
    pThreat(BlockValue, Preciousness, ProbT),
    Threat is ProbT / 0.5,
    addThreats(PlayerListTail, Preciousness, EvaluatedPosition, OtherThreats),
    TotalThreat is Threat + OtherThreats.
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

maxLayingBlock([], [0, _BlockPos]).
maxLayingBlock([[B, X, Y]|[]], [B, [X,Y]]) :- !.
maxLayingBlock([[B, X, Y]|BlockListTail], MaxBlock) :-
    maxLayingBlock(BlockListTail, SubsequentMaxBlock),
    greaterBlock([B, [X,Y]], SubsequentMaxBlock, MaxBlock).

greaterBlock([BlockValue1, _BlockPos1], [BlockValue2, BlockPos2], [BlockValue2, BlockPos2]) :-
    BlockValue2 > BlockValue1, !.
greaterBlock(Block1, _Block2, Block1).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Data Structures
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%------------------------------------------------------------------------------
% Structure de données de file (queue) et file (queue) avec priorité
% Auteur: Charles-Antoine Brunet
%------------------------------------------------------------------------------
% Version 1.0: Version initiale
% Date: 2005/04/11
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% +: paramètre en entrée
% -: paramètre en sortie
% ?: paramètre en entrée ou sortie
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Tester si une file est vide ou créer une file
% empty_queue(?Stack)
%------------------------------------------------------------------------------
empty_queue([]).

%------------------------------------------------------------------------------
% Ajouter un item dans la file
% enqueue(+Item, +Queue, -NewQueue)
% Item=item à ajouter, Y=ancienne file, Z=nouvelle file
%------------------------------------------------------------------------------
enqueue(E, [], [E]).
enqueue(E, [H|T], [H|Tnew]) :- enqueue(E, T, Tnew).

%------------------------------------------------------------------------------
% Elever un item de la file
% dequeue(-Item, +Queue, -NewQueue)
% Item= item enlevé, Queue=ancienne file, NewQueue=la nouvelle file
%------------------------------------------------------------------------------
dequeue(E, [E|T], T).

%------------------------------------------------------------------------------
% Consulte le premier item de la file
% peek_queue(-Item, +Queue), Item=premier item, Queue= file a consulter
%------------------------------------------------------------------------------
peek_queue(E, [E|_]).

%------------------------------------------------------------------------------
% Vérifier si un élement est membre d'une file
% Utilise la fonction member de la librairie standard de liste
%------------------------------------------------------------------------------
member_queue(E, T) :- member(E, T).

%------------------------------------------------------------------------------
% Ajoute une liste d'élements à une file
% add_list_to_queue(+List, +Queue, -NewQueue)
% List=liste à ajouter, Queue=ancienne file, NewQueue=nouvelle file
% Utilise la fonction append de la librairie standard de liste
%------------------------------------------------------------------------------
add_list_to_queue(List, T, NewT) :- append(T, List, NewT).
%------------------------------------------------------------------------------
% QUEUE AVEC PRIORITÉ
%------------------------------------------------------------------------------
% Les opérateurs empty_queue, member_queue, dequeue et peek sont les mêmes
%      que plus haut. Les 2 opérateurs qui changent sont les suivants
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Ajouter un item dans la file avec priorité
% insert_pq(+Item, +Queue, -NewQueue)
% Item=item à ajouter, Y=ancienne file, Z=nouvelle file
%------------------------------------------------------------------------------
insert_pq(E, [], [E]) :- !.
insert_pq(E, [H|T], [E, H|T]) :- precedes(E,H), !.
insert_pq(E, [H|T], [H|Tnew]) :- insert_pq(E, T, Tnew).

%------------------------------------------------------------------------------
% Ajouter une liste d'éléments (non ordonnés) à une file avec priorité
% insert_list_pq(+List, +Queue, -NewQueue)
% List=liste à ajouter, Queue=ancienne file, NewQueue=nouvelle file
%------------------------------------------------------------------------------
insert_list_pq([], L, L).
insert_list_pq([E|T], L, NewL) :-
    insert_pq(E, L, Tmp), insert_list_pq(T, Tmp, NewL).

%------------------------------------------------------------------------------
% IMPORTANT! Selon le type de données, peut-être nécessaire de changer la
%     définition du prédicat suivant.
%------------------------------------------------------------------------------
%precedes(X,Y) :- X < Y.
