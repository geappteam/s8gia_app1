:- module(p06,[p06_nom/1,p06_auteurs/1,p06_reset/0,p06_plan/1,p06_action/2]).
:- ensure_loaded(state_ops).

% Indetification
p06_nom('Chuckitty').
p06_auteurs('Goku & Eddy').

% Jeux
p06_reset :- true.
p06_plan([none]).
p06_action(_Etat, none).
