un_sur_deux([]).
un_sur_deux([_]).
un_sur_deux([_,Second|Tail]) :-
    write(Second),nl,
    un_sur_deux(Tail).
