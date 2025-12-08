% Créer un nouveau plateau avec un coup joué
playMove(Move, Player, NewCol) :-
    column(Move, ColData, LastPos),
    NewPos is LastPos +1,
    replace_nth1(ColData, NewPos, Player, NewColData),
    retract(column(Move,ColData,LastPos)),
    assert(column(Move,NewColData,NewPos)).

replace_nth1(List, Index, Elem, NewList) :-
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, Elem, Rest).