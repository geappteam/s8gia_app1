%
% List Commands
%

% Command 1 : Append
executeCommand(1, Valeur, [], [Valeur]).
executeCommand(1, Valeur, [InList|T], [InList|OutList]) :-
    executeCommand(1, Valeur, T, OutList).

% Command 2 : Remove first occurence
executeCommand(2, Target, [], []) :-
    format('Element ~w not found.~n', Target).
executeCommand(2, Target, [Target|Rest], Rest).
executeCommand(2, Target, [NonMatch|Rest], [NonMatch|Tail]) :-
    Target =\= NonMatch,
    executeCommand(2, Target, Rest, Tail).

% Command 3 : Remove all occurences
executeCommand(3, _, [], []).
executeCommand(3, Target, [Target|Rest], Tail) :-
    executeCommand(3, Target, Rest, Tail).
executeCommand(3, Target, [NonMatch|Rest], [NonMatch|Tail]) :-
    Target =\= NonMatch,
    executeCommand(3, Target, Rest, Tail).

% Command 4 : Duplicate value at specified index
executeCommand(4, 0, [Duplicable|Tail], [Duplicable,Duplicable|Tail]).
executeCommand(4, _, [], []) :-
    write('Index out of bounds'), nl, fail.
executeCommand(4, Index, [Iter|Rest], [Iter|Tail]) :-
    Index > 0,
    NewIndex is Index -1,
    executeCommand(4, NewIndex, Rest, Tail).






%
% List instructions processing
%

% Empty instruction list
gerer_liste([], List, List).

% Ignoring zeros
gerer_liste([0|Tail], IL, OL) :-
    gerer_liste(Tail, IL, OL).

% Failing for orphan operators
gerer_liste([Orphan], List, List) :-
    Orphan =\= 0,
    write('Missing operand'), fail.

% Recursive processing of elements
gerer_liste([Operator,Operand|Tail], InList, OutList) :-
    executeCommand(Operator, Operand, InList, ModifiedList),
    gerer_liste(Tail, ModifiedList, OutList).
