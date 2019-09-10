:- module(p06,[p06_nom/1,p06_auteurs/1,p06_reset/0,p06_plan/1,p06_action/2]).
:- ensure_loaded(planning).
:- ensure_loaded(actions).
:- ensure_loaded(benefice).

% Identification
p06_nom('Chuckitty').
p06_auteurs('Goku & Eddy').

% Jeux
p06_reset :-
    initPlan(P),
    setPlan(P).
p06_plan(P) :-
    getPlan(P).
p06_action(State, Action) :-
    getAction(State, Action).

% Planning
:- dynamic([restOfPlan/1]).
initPlan(P) :-
    p06_action(State, _),
    make_plan(State, [], P).
restOfPlan(P) :-
    p06_action(State, _),
    make_plan(State, [], P).
getPlan(State,P) :-
    with_mutex(p06, restOfPlan(P)),
    make_plan(State, [], P).
setPlan(P) :-
    with_mutex(p06, changePlan(P)).
changePlan(P) :-
    retractall(restOfPlan(_)),
    assert(restOfPlan(P)).

% Actions
%getAction(State, NextAction) :-
%    getPlan(State, [NextAction]), !, initPlan(P), setPlan(P).
getAction(State, NextAction) :-
    getPlan(State, [NextAction|RestPlan]), setPlan(RestPlan).

% Fake test plan
testPlan([4,3,4,4,[[2,'Brutus',0,2,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Chuckitty',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]]).







