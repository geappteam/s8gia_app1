% Actions definitions
% + Any of the five possible actions.
% Helps categorize 'actions with any directions'.
is_action(move(Direction)).
is_action(attack(Direction)).
is_action(drop(Direction)).
is_action(take(Direction)).
is_action(none).

% + InitialState : Current environment's state.
% + Empty pile : Starts a pile of duos of any action and its benefit has
% first element of the pile
pile_best_actions(InitState, []) :-
    benefice(is_action(AnyAction), InitState, Benefice),
    pile_best_actions(InitState, [[AnyAction|Benefice]|[]]).

% + InitialState : Current environment's state.
% - BestActionsPile : Pile of all possible actions after adding
% another best action at the top
pile_best_actions(InitState, BestActionsPile) :-
    benefice(is_action(BestAction), InitState, NewBenefice),
    peek_stack(BestActionsPile,[[TopAction|TopBenefice]|_]),
    TopBenefice < NewBenefice,
    pile_best_actions(InitState, [[BestAction|NewBenefice]|BestActionsPile]).

% + InitialState : Current environment's state.
% + BestActionsPile : Pile of all possible actions with the best action
% at the top
pile_best_actions(InitState, BestActionsPile) :-
    benefice(is_action(BestAction), InitState, NewBenefice),
    peek_stack(BestActionsPile,[[TopAction|TopBenefice]|_]),
    TopBenefice >= NewBenefice,
    pile_best_actions(InitState, BestActionsPile).

% + InitState : Initial state of the environment
% + Goal : To validate as last element of the ActionPlan queue
% - ModifiedState : New state obtained after
% - ActionPlan : Queue of actions to do
%plan_graph(InitState, Goal, ModifiedState, ActionPlan) :-
    % Solve which action has the best benefit
    % TODO...

    % The state of the environment has been changed
    % TODO...

    % The action plan has been changed
    % TODO ...

    % Recusivity
    % TODO...
