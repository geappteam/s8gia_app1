:- ensure_loaded('../lib/stack').
:- ensure_loaded(actions).

get_best_action([Action,Benefice],[Action,Benefice]).

get_best_action([[_TopAction,TopBenefice],[SecondAction,SecondBenefice]], [[SecondAction,SecondBenefice]]) :-
    TopBenefice < SecondBenefice.

get_best_action([[TopAction,TopBenefice],[_SecondAction,SecondBenefice]], [TopAction,TopBenefice]) :-
    TopBenefice >= SecondBenefice.

get_best_action([[_TopAction,TopBenefice],[SecondAction,SecondBenefice]|Tail], BestActionBenefice) :-
    TopBenefice < SecondBenefice,
    %potential debug :add append with tail first?
    order_list_top_benefice([[SecondAction,SecondBenefice],Tail],BestActionBenefice).

get_best_action([[TopAction|TopBenefice],[_SecondAction|SecondBenefice]|Tail], BestActionBenefice) :-
    TopBenefice >= SecondBenefice,
    %potential debug :add append with tail first?
    order_list_top_benefice([[TopAction|TopBenefice],Tail],BestActionBenefice).

% + InitState : Initial state of the environment.
% + ActionPlan : Queue of actions to do so far.
% - NewActionPlan : New queue of actions to do.
% Note : Outputs NewActionPlan equals to ActionPlan. Stops recursivity
% once 'none' action appears as last best action to do.
make_plan(_InitState, Plan, Plan) :-
     member_stack(none, Plan).

% + InitState : Initial state of the environment.
% + ActionPlan : Empty queue of actions to do so far.
% - NewActionPlan : New queue of actions to do.
% Note : Initialized ActionPlan when the queue is empty.
make_plan(InitState, [], NewActionPlan) :-
    % Solve which action has the best benefit giving the initial state
    findall(ActionBenefice, benefice(ActionBenefice, InitState, _Benefice), AllPotentialActionsBenefice),
    get_best_action(AllPotentialActionsBenefice, [BestAction|_BestBenefice]),

    % The state of the environment has been changed
    modify_state(BestAction, InitState, InitState2),

    % Recursivity & add next action to the action plan
    make_plan(InitState2, [BestAction], NewActionPlan).

% + InitState : Initial state of the environment.
% + ActionPlan : Queue of actions to do.
% - NewActionPlan : New queue of actions to do.
make_plan(InitState, InitialPlan, NewActionPlan) :-
    \+ member_stack(none, InitialPlan),

    % Solve which action has the best benefit giving the initial state
    findall(ActionBenefice, benefice(ActionBenefice, InitState, _Benefice), AllPotentialActionsBenefice),
    get_best_action(AllPotentialActionsBenefice, [BestAction|_BestBenefice]),

    % The state of the environment has been changed
    modify_state(BestAction, InitState, InitState2),

    % Append new action at the end
    append([BestAction], InitialPlan, IncrementalPlan),

    % Recursivity & add next action to the action plan
    make_plan(InitState2, IncrementalPlan, NewActionPlan).

















