% Actions definitions
% + Any of the five possible actions.
% Note : Helps categorize 'actions with any directions'.
is_action(move(Direction)).
is_action(attack(Direction)).
is_action(drop(Direction)).
is_action(take(Direction)).
is_action(none).

% + InitialState : Current environment's state.
% + Empty pile : Starts a pile of duos of any action and its benefit has
% first element of the pile.
pile_best_actions(InitState, []) :-
    benefice(is_action(AnyAction), InitState, Benefice),
    pile_best_actions(InitState, [[AnyAction|Benefice]|[]]).

% + InitialState : Current environment's state.
% - BestActionsPile : Pile of all possible actions after adding.
% another best action at the top.
% Note : Will add to the pile.
pile_best_actions(InitState, BestActionsPile) :-
    benefice(is_action(BestAction), InitState, NewBenefice),
    peek_stack(BestActionsPile,[[TopAction|TopBenefice]|_]),
    TopBenefice < NewBenefice,
    pile_best_actions(InitState, [[BestAction|NewBenefice]|BestActionsPile]).

% + InitialState : Current environment's state.
% + BestActionsPile : Pile of all possible actions with the best action
% at the top.
% Note : Will not add to the pile.
pile_best_actions(InitState, BestActionsPile) :-
    benefice(is_action(BestAction), InitState, NewBenefice),
    peek_stack(BestActionsPile,[[TopAction|TopBenefice]|_]),
    TopBenefice >= NewBenefice,
    pile_best_actions(InitState, BestActionsPile).

% + InitialState : Current environment's state.
% + BestActionsPile : Pile of all possible actions with the best action
% at the top.
% Note : Ensure to stop piling when all actions-of-all-directions'
% benefits have been analyzed.
pile_best_actions(InitState, BestActionsPile) :-
    benefice(is_action(BestAction), InitState, NewBenefice),
    peek_stack(BestActionsPile,[[TopAction|TopBenefice]|_]),
    TopBenefice >= NewBenefice,

    member_stack([move(1)|_], BestActionsPile),
    member_stack([move(2)|_], BestActionsPile),
    member_stack([move(3)|_], BestActionsPile),
    member_stack([move(4)|_], BestActionsPile),
    member_stack([move(5)|_], BestActionsPile),
    member_stack([move(6)|_], BestActionsPile),
    member_stack([move(7)|_], BestActionsPile),
    member_stack([move(8)|_], BestActionsPile),

    member_stack([attack(1)|_], BestActionsPile),
    member_stack([attack(2)|_], BestActionsPile),
    member_stack([attack(3)|_], BestActionsPile),
    member_stack([attack(4)|_], BestActionsPile),
    member_stack([attack(5)|_], BestActionsPile),
    member_stack([attack(6)|_], BestActionsPile),
    member_stack([attack(7)|_], BestActionsPile),
    member_stack([attack(8)|_], BestActionsPile),

    member_stack([drop(1)|_], BestActionsPile),
    member_stack([drop(2)|_], BestActionsPile),
    member_stack([drop(3)|_], BestActionsPile),
    member_stack([drop(4)|_], BestActionsPile),
    member_stack([drop(5)|_], BestActionsPile),
    member_stack([drop(6)|_], BestActionsPile),
    member_stack([drop(7)|_], BestActionsPile),
    member_stack([drop(8)|_], BestActionsPile),

    member_stack([take(1)|_], BestActionsPile),
    member_stack([take(2)|_], BestActionsPile),
    member_stack([take(3)|_], BestActionsPile),
    member_stack([take(4)|_], BestActionsPile),
    member_stack([take(5)|_], BestActionsPile),
    member_stack([take(6)|_], BestActionsPile),
    member_stack([take(7)|_], BestActionsPile),
    member_stack([take(8)|_], BestActionsPile),

    member_stack([none|_], BestActionsPile).

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
