% Conditions de victoire (optimisées)

% Détecter une victoire horizontale
horizontalVictory(Player, Column) :-
    column(Column, _, Row),
    between(0, 3, Offset),
    StartCol is Column - Offset,
    StartCol >= 1,
    EndCol is StartCol + 3,
    EndCol =< 7,
    forall(between(StartCol, EndCol, Col),
        (
            column(Col, ColData, _),
            nth1(Row, ColData, Player) % ColData[Row - 1] == Player
        )
    ),
    !.

% Détecter une victoire verticale
verticalVictory(Player, Column) :-
    column(Column, ColData, Row),
    between(0, 3, Offset),
    StartRow is Row - Offset,
    StartRow >= 1,
    EndRow is StartRow + 3,
    EndRow =< 6,
    forall(between(StartRow, EndRow, R),
        (
            nth1(R, ColData, Player)
        )
    ),
    !.

% Détecter une victoire diagonale ↗
rightDiagonalVictory(Player, Column) :-
    column(Column, _, Row),
    between(0, 3, Offset),
    StartRow is Row - Offset,
    StartCol is Column - Offset,
    StartRow >= 1,
    StartCol >= 1,
    EndRow is StartRow + 3,
    EndCol is StartCol + 3,
    EndRow =< 6,
    EndCol =< 7,
    forall(between(0, 3, I),
        (
            R is StartRow + I,
            C is StartCol + I,
            column(C, ColData, _),
            nth1(R, ColData, Player)
        )
    ),
    !.

% Détecter une victoire diagonale ↖
leftDiagonalVictory(Player, Column) :-
    column(Column, _, Row),
    between(0, 3, Offset),
    StartRow is Row - Offset,
    StartCol is Column + Offset,
    StartRow >= 1,
    StartCol =< 7,
    EndRow is StartRow + 3,
    EndCol is StartCol - 3,
    EndRow =< 6,
    EndCol >= 1,
    forall(between(0, 3, I),
        (
            R is StartRow + I,
            C is StartCol - I,
            column(C, ColData, _),
            nth1(R, ColData, Player)
        )
    ),
    !.

% Conditions de victoire (non optimisées)

% Détecter une victoire horizontale
% horizontalVictory(Player) :-
%     between(1, 6, Row),
%     between(1, 4, StartCol),
%     EndCol is StartCol + 3,
%     forall(between(StartCol, EndCol, Col),
%         (
%             column(Col, ColData, _),
%             nth1(Row, ColData, Player)
%         )
%     ).

% Détecter une victoire verticale
% verticalVictory(Player) :-
%     between(1, 7, Col),
%     column(Col, ColData, _),
%     between(1, 3, StartRow),
%     EndRow is StartRow + 3,
%     forall(between(StartRow, EndRow, Row),
%         (
%             nth1(Row, ColData, Player)
%         )
%     ).

% Détecter une victoire diagonale ↗
% rightDiagonalVictory(Player) :-
%     between(1, 3, StartRow),
%     between(1, 4, StartCol),
%     forall(between(0, 3, Offset),
%         (
%             Row is StartRow + Offset,
%             Col is StartCol + Offset,
%             column(Col, ColData, _),
%             nth1(Row, ColData, Player)
%         )
%     ).

% Détecter une victoire diagonale ↖
% leftDiagonalVictory(Player) :-
%     between(1, 3, StartRow),
%     between(4, 7, StartCol),
%     forall(between(0, 3, Offset),
%         (
%             Row is StartRow + Offset,
%             Col is StartCol - Offset,
%             column(Col, ColData, _),
%             nth1(Row, ColData, Player)
%         )
%     ).

% Fin de partie
% Appeler avant de changer de joueur

isOver(Player, Column) :-
    (   horizontalVictory(Player, Column)
    ;   verticalVictory(Player, Column)
    ;   leftDiagonalVictory(Player, Column)
    ;   rightDiagonalVictory(Player, Column)
    ),
    !.
    %write(Player), writeln(' has won the match !').

isOver(_, _) :-
    isTie().
    % writeln('It\'s a tie !').

isTie() :-
    \+ (column(_, _, LastPos), LastPos < 6).