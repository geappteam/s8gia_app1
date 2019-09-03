un_sur_deux([]).
un_sur_deux([_]).
un_sur_deux([_,Second|Tail]) :-
    write(Second),
    un_sur_deux(Tail).
