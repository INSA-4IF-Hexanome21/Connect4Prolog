convertSymbol('RED', '🔴').
convertSymbol('YELLOW', '🟡').
convertSymbol('e', '⬜').
convertSymbol(X, X).
convertPlayer(1, 'RED').
convertPlayer(2, 'YELLOW').

% Afficher le plateau
displayBoard :-
    nl,
    % On parcourt les lignes de 0 (haut) à 5 (bas)
    forall(between(1,6,Row),
        (
            % Pour chaque colonne de 0 à 6
            forall(between(1,7,Col),
                (
                    column(Col, ColData,LastPos),
                    Pos is 7-Row,
                    nth1(Pos, ColData, Cell),
                    convertSymbol(Cell, Symbol),
                    write(Symbol), write('||')
                )
            ),
            nl
        )
    ),
    write('1   2   3   4   5   6   7   '), nl, nl.