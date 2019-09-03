executeCommande(1, Valeur, [], [Valeur]).
executeCommande(1, Valeur, [InList|Empty], [InList|OutList]) :-
    executeCommande(1, Valeur, Empty, OutList).



gerer_liste([], List, List).
gerer_liste([0|Tail], IL, OL) :-
    gerer_liste(Tail, IL, OL).
gerer_liste([_], List, List) :-
    write('Missing operand'),
    false.
gerer_liste([Operator,Operand|Tail], InputList, OutputList) :-
    executeCommande(Operator, Operand, InputList, ModifiedList),
    gerer_liste(Tail, ModifiedList, OutputList).


