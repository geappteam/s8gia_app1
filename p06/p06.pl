:- module(p06,[p06_nom/1,p06_auteurs/1,p06_reset/0,p06_plan/1,p06_action/2]).
:- ensure_loaded(planner).
:- ensure_loaded(actions).
:- ensure_loaded(benefice).



% Indentification
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
    setPlan(CompletePlan).



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
