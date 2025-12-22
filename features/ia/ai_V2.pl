aiV2(Move,_) :-
    (
        (verifCol(Move),!);
        (verifRow(Move),!);
        (verifDiagLeft(Move),!);
        (verifDiagRight(Move),!)
    ;
    (   
    	repeat,
        random(1,8,Move),
        column(Move,_,IndexMax),
        not(IndexMax == 6),
        !
    )
    ),
    format(user_error, "AI V2 Play : ~w~n",[Move]).

verifCol(Move) :- between(1,7,Move),
        (
            column(Move,Col,LastPos),
            LastPos > 2,
            Index1 is LastPos,
            Index2 is LastPos - 1,
            Index3 is LastPos - 2,
            nth1(Index1,Col,Val),
            nth1(Index2,Col,Val),
            nth1(Index3,Col,Val),
            !            
        ).

threeAligned([X,X,X,e]) :- X \= e.
threeAligned([X,X,e,X]) :- X \= e.
threeAligned([X,e,X,X]) :- X \= e.
threeAligned([e,X,X,X]) :- X \= e.

cell(Col, Row, Val) :-
    column(Col, ColData, _),
    nth1(Row, ColData, Val).

canPlay(Col, Row) :-
    LastPos is Row - 1,
    column(Col,_,LastPos). 


% Recherche du coup gagnant en diagonale ↗
verifRow(Move) :-
    
    between(1,6,Row),
    between(1,4,Col),

    Col1 is Col + 1,
    Col2 is Col + 2,
    Col3 is Col + 3,

    % bord du plateau
    Col3 =< 7,

    % Lire les 4 valeurs
    cell(Col , Row, V0),
    cell(Col1, Row, V1),
    cell(Col2, Row, V2),
    cell(Col3, Row, V3),

    Pattern = [V0, V1, V2, V3],

    % Tester l’alignement "3 mêmes + 1 vide"
    threeAligned(Pattern),

    % Trouver la position de la case vide dans le pattern
    nth1(Pos, Pattern, e),

    % Convertir Pos → colonne du coup à jouer
    nth1(Pos, [Col,Col1,Col2,Col3], Move),

    % Vérifier que jouer dans cette colonne est possible
    canPlay(Move, Row),
    !.


% Recherche du coup gagnant en diagonale ↗
verifDiagLeft(Move) :-
    
    between(1,6,Row),
    between(4,7,Col),

    Col1 is Col - 1, Row1 is Row + 1,
    Col2 is Col - 2, Row2 is Row + 2,
    Col3 is Col - 3, Row3 is Row + 3,

    % bord du plateau
    Col3 =< 7,
    Col3 >= 1,
    Row3 >= 1,

    % Lire les 4 valeurs
    cell(Col , Row , V0),
    cell(Col1, Row1, V1),
    cell(Col2, Row2, V2),
    cell(Col3, Row3, V3),

    Pattern = [V0, V1, V2, V3],

    % Tester l’alignement "3 mêmes + 1 vide"
    threeAligned(Pattern),

    % Trouver la position de la case vide dans le pattern
    nth1(Pos, Pattern, e),

    % Convertir Pos → colonne du coup à jouer
    nth1(Pos, [Col,Col1,Col2,Col3], Move),
    nth1(Pos, [Row,Row1,Row2,Row3], RowPlayed),

    % Vérifier que jouer dans cette colonne est possible
    canPlay(Move, RowPlayed),
    !.

verifDiagRight(Move) :-
    % Points de départ possibles
    between(1,3,Row),      
    between(1,7,Col),      

    Col1 is Col + 1, Row1 is Row + 1,
    Col2 is Col + 2, Row2 is Row + 2,
    Col3 is Col + 3, Row3 is Row + 3,

    % bord du plateau
    Col3 =< 7,
    Row3 =< 6,

    % Lire les 4 valeurs
    cell(Col , Row , V0),
    cell(Col1, Row1, V1),
    cell(Col2, Row2, V2),
    cell(Col3, Row3, V3),

    Pattern = [V0, V1, V2, V3],

    % Tester l’alignement "3 mêmes + 1 vide"
    threeAligned(Pattern),

    % Trouver la position de la case vide dans le pattern
    nth1(Pos, Pattern, e),

    % Convertir Pos → colonne du coup à jouer
    nth1(Pos, [Col,Col1,Col2,Col3], Move),
    nth1(Pos, [Row,Row1,Row2,Row3], RowPlayed),

    % Vérifier que jouer dans cette colonne est possible
    canPlay(Move, RowPlayed),
    !.