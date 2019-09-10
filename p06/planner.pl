:- ensure_loaded(actions).
:- ensure_loaded('../lib/queue').




% Used in priority queue
precedes([Cost1, _State1, _Parent1], [Cost2, _Stat2, _Parent2]) :- Cost1 < Cost2.




% bestFistPlan(+OriginalState, :SuccessorExplorer, :Goal, :Heuristic, -Plan)
bestFirstPlan(OriginalState, SuccessorExplorer, Goal, Heuristic, Plan) :-
    % Element in this queue : [Cost, State, Parent]
    OpenPq = [[0, OriginalState, none]],
    ClosedSet = [],
    bestFirstLoop(OpenPq, ClosedSet, SuccessorExplorer, Goal, Heuristic, StateList),
    makeActionList(SuccessorExplorer, StateList, Plan).

bestFirstLoop([[C, State, P]|OpenSet], ClosedSet, _S, Goal, _F, StateList) :-
    call(Goal, State), !,
    buildStateList(State, [[C, State, P]|ClosedSet], InvStateList),
    reverse(InvStateList, StateList).
bestFirstLoop([Node|OpenSet], ClosedSet, SuccessorExplorer, Goal, Heuristic, StateList) :-
    Node = [_NodeCost, NodeState, _NodeParent],
    findall(S, call(SuccessorExplorer, NodeState, S), SuccessorList),
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
    \+ member([OldCost, SState, OldParent], ClosedSet),
    \+ member([OldCost, SState, OldParent], OpenSet),!,
    Parent = [ParentCost, ParentState, _ParentParent],
    call(Heuristic, SState, StateCost),
    NewCost is StateCost + ParentCost,
    insert_pq([NewCost, SState, ParentState], OpenSet, OpenSet1),
    handleSuccessors(SList, OpenSet1, ClosedSet, Parent, Heuristic, OpenSetOut, ClosedSetOut).
handleSuccessors([SState|SList], OpenSet, ClosedSet, Parent, Heuristic, OpenSetOut, ClosedSetOut) :-
    handleSuccessors(SList, OpenSet, ClosedSet, Parent, Heuristic, OpenSetOut, ClosedSetOut).
