% Actions definitions
% + Any of the five possible actions.
% Note : Helps categorize 'actions with any directions'.
is_action(move(Direction)).
is_action(attack(Direction)).
is_action(drop(Direction)).
is_action(take(Direction)).
is_action(none).

% + InitialState : Current environment's state.
% + [] : Empty BestActionsStack stack which starts a stack of duos of
% any action and its benefit has first element of the stack.
stack_best_actions(InitState, []) :-
    benefice(is_action(AnyAction), InitState, Benefice),
    stack_best_actions(InitState, [[AnyAction|Benefice]]).

% + InitialState : Current environment's state.
% - BestActionsStack : Stack of all possible actions after adding.
% another best action at the top.
% Note : Will add to the stack.
stack_best_actions(InitState, BestActionsStack) :-
    benefice(is_action(BestAction), InitState, NewBenefice),
    peek_stack(BestActionsStack,[[TopAction|TopBenefice]|_]),
    TopBenefice < NewBenefice,
    stack_best_actions(InitState, [[BestAction|NewBenefice]|BestActionsStack]).

% + InitialState : Current environment's state.
% + BestActionsStack : Stack of all possible actions with the best
% action at the top.
% Note : Will not add to the stack.
stack_best_actions(InitState, BestActionsStack) :-
    benefice(is_action(BestAction), InitState, NewBenefice),
    peek_stack(BestActionsStack,[[TopAction|TopBenefice]|_]),
    TopBenefice >= NewBenefice,
    stack_best_actions(InitState, BestActionsStack).

% + InitialState : Current environment's state.
% + BestActionsStack : Stack of all possible actions with the best
% action at the top.
% Note : Ensure to stop stacking when all
% actions-of-all-directions' benefits have been analyzed.
stack_best_actions(InitState, BestActionsStack) :-
    benefice(is_action(BestAction), InitState, NewBenefice),
    peek_stack(BestActionsStack,[[TopAction|TopBenefice]|_]),
    TopBenefice >= NewBenefice,

    member_stack([move(1)|_], BestActionsStack),
    member_stack([move(2)|_], BestActionsStack),
    member_stack([move(3)|_], BestActionsStack),
    member_stack([move(4)|_], BestActionsStack),
    member_stack([move(5)|_], BestActionsStack),
    member_stack([move(6)|_], BestActionsStack),
    member_stack([move(7)|_], BestActionsStack),
    member_stack([move(8)|_], BestActionsStack),

    member_stack([attack(1)|_], BestActionsStack),
    member_stack([attack(2)|_], BestActionsStack),
    member_stack([attack(3)|_], BestActionsStack),
    member_stack([attack(4)|_], BestActionsStack),
    member_stack([attack(5)|_], BestActionsStack),
    member_stack([attack(6)|_], BestActionsStack),
    member_stack([attack(7)|_], BestActionsStack),
    member_stack([attack(8)|_], BestActionsStack),

    member_stack([drop(1)|_], BestActionsStack),
    member_stack([drop(2)|_], BestActionsStack),
    member_stack([drop(3)|_], BestActionsStack),
    member_stack([drop(4)|_], BestActionsStack),
    member_stack([drop(5)|_], BestActionsStack),
    member_stack([drop(6)|_], BestActionsStack),
    member_stack([drop(7)|_], BestActionsStack),
    member_stack([drop(8)|_], BestActionsStack),

    member_stack([take(1)|_], BestActionsStack),
    member_stack([take(2)|_], BestActionsStack),
    member_stack([take(3)|_], BestActionsStack),
    member_stack([take(4)|_], BestActionsStack),
    member_stack([take(5)|_], BestActionsStack),
    member_stack([take(6)|_], BestActionsStack),
    member_stack([take(7)|_], BestActionsStack),
    member_stack([take(8)|_], BestActionsStack),

    member_stack([none|_], BestActionsStack).

% + InitState : Initial state of the environment.
% - BestAction : Best action possible among all possible actions giving
% the current environment's state.
get_best_action(InitState, BestAction) :-
    stack_best_actions(InitState, [[BestAction|_]|_]).

% + InitState : Initial state of the environment.
% - ActionPlan : Queue of actions to do.
%plan_graph(InitState, ActionPlan) :-
    % Solve which action has the best benefit giving the initial state
    %get_best_action(InitState, BestAction),

    % The state of the environment has been changed
    %modify_state(BestAction, InitState, NextInitialState),

    % Add next action to the action plan
    %stack(BestAction, ActionPlan, NewActionPlan),

    % Check if top action plan is not none
    %BestAction =/= none,

    % Recursivity
    %plan_graph(NextInitialState, NewActionPlan).

% + InitState : Initial state of the environment
% - ModifiedState : New state obtained after
% - ActionPlan : Queue of actions to do
%plan_graph(_, [none|_]).
















