:- module(p06,[p06_nom/1,p06_auteurs/1,p06_reset/0,p06_plan/1,p06_action/2]).
:- ensure_loaded(planning).
:- ensure_loaded(actions).
:- ensure_loaded(benefice).

% Indetification
p06_nom('Chuckitty').
p06_auteurs('Goku & Eddy').

% Jeux
p06_reset :- true.
p06_plan([none]).
p06_action(_Etat, none).


% Fake test plan
testPlan([4,3,4,4,[[2,'Brutus',0,2,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Chuckitty',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]]).
