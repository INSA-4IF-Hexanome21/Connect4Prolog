%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------------   VARIABLES   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic column/3.
:- dynamic player/2.
:- dynamic currentPlayer/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------   CONDITIONS DE VICTOIRE   ------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% Détecter une victoire horizontale
horizontalVictory(Player) :-
    between(1, 6, Row),
    between(1, 4, StartCol),
    EndCol is StartCol + 3,
    forall(between(StartCol, EndCol, Col),
        (
            column(Col, ColData, _),
            nth1(Row, ColData, Player)
        )
    ).

% Détecter une victoire verticale
verticalVictory(Player) :-
    between(1, 7, Col),
    column(Col, ColData, _),
    between(1, 3, StartRow),
    EndRow is StartRow + 3,
    forall(between(StartRow, EndRow, Row),
        (
            nth1(Row, ColData, Player)
        )
    ).

% Détecter une victoire diagonale ↖
leftDiagonalVictory(Player) :-
    between(1, 3, StartRow),
    between(1, 4, StartCol),
    % EndRow is StartRow + 3,
    % EndCol is StartCol + 3,
    forall(between(0, 3, Offset),
        (
            Row is StartRow + Offset,
            Col is StartCol + Offset,
            column(Col, ColData, _),
            nth1(Row, ColData, Player)
        )
    ).

% Détecter une victoire diagonale ↗
rightDiagonalVictory(Player) :-
    between(1, 3, StartRow),
    between(4, 7, StartCol),
    % EndRow is StartRow + 3,
    % EndCol is StartCol - 3,
    forall(between(0, 3, Offset),
        (
            Row is StartRow + Offset,
            Col is StartCol - Offset,
            column(Col, ColData, _),
            nth1(Row, ColData, Player)
        )
    ).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------   AFFICHAGE DU PLATEAU   --------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_symbol('RED', '🔴').
convert_symbol('YELLOW', '🟡').
convert_symbol('e', '⬜').   
convert_symbol(X, X).   

%Afficher Le Plateau
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
                    convert_symbol(Cell, Symbol),
                    write(Symbol), write('||')
                )
            ),
            nl
        )
    ),
    write('1   2   3   4   5   6   7   '), nl, nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------------   IA   -------------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Trouver Le Meilleur Mouvement
%Faire Un Mouvement Aleatoire
ia(Move,_) :- 
    repeat,
    random(1,8,Move),
    column(Move,Col,IndexMax),
    not(IndexMax == 6),
    !.

askPlayerMove(Move,_) :-
    repeat,
    writeln('Where do you want to play ? '),
    read(Entry),
    (   integer(Entry), Entry>0, Entry<8 ->
        Move = Entry
    ;
        writeln('Invalid option.'), fail
    ),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ------------------ JOUER UN COUP  --------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Creer Un Nouveau Plateau Avec Le Nouveau Mouv
playMove(Move, Player, NewCol) :-
    column(Move, ColData, LastPos),
    NewPos is LastPos +1,
    replace_nth1(ColData, NewPos, Player, NewColData),
    retract(column(Move,ColData,LastPos)),
    assert(column(Move,NewColData,NewPos)).

replace_nth1(List, Index, Elem, NewList) :-
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, Elem, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   FIN DE PARTIE   --------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Appeler avant de changer de joueur
isOver(Player) :-
    (   horizontalVictory(Player)
    ;   verticalVictory(Player)
    ;   leftDiagonalVictory(Player)
    ;   rightDiagonalVictory(Player)
    ),
    !,
    write(Player), writeln(' has won the match !').

isOver(_) :-
    isTie(),
    writeln('It\'s a tie !').

isTie() :-
    \+ (column(_, _, LastPos), LastPos =< 6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   CHANGER DE JOUEUR   ---------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nextPlayer :-
    currentPlayer(Current),
    Current = player(Color, _),
    ( Color = 'RED' -> player('YELLOW', Next)
    ; Color = 'YELLOW' -> player('RED', Next)
    ),
    retract(currentPlayer(Current)),
    assert(currentPlayer(Next)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   BOUCLE DE JEU   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play :-
    currentPlayer(Player),
    Player = player(Color,Type),
    write('New turn for: '), writeln(Color),
    write('playerType: '), writeln(Type),

    ( Type == ai -> ia(Move, Player)
    ; askPlayerMove(Move, Player)
    ),

    playMove(Move, Color, _),
    displayBoard,

    ( isOver(Color) -> true
    ; nextPlayer,
      play
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   INITIALISATION   ------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
selectPlayerType(Type) :-
    writeln('1 - ai  '),
    writeln('2 - player '),
    read(Entry),
    (   Entry =:= 1 -> Type = 'ai'
    ;   Entry =:= 2 -> Type = 'human'
    ;   writeln('Invalid option.'), fail
    ).

initBoard :-
    retractall(column(_,_,_)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    initPlayer(PlayerR, PlayerJ),
    displayBoard,
    play.

initPlayer(PlayerR, PlayerJ) :-
    writeln('--- Red Player (RED) ---'),
    selectPlayerType(TypeR),
    nl, write('Red Player is '), writeln(TypeR),

    writeln('--- Yellow Player (YELLOW) ---'),
    selectPlayerType(TypeJ),
    nl, write('Yellow Player is '), writeln(TypeJ),

    PlayerR = player('RED',TypeR),
    assert(player('RED', TypeR)),
    PlayerJ = player('YELLOW',TypeJ),
    assert(player('YELLOW',TypeJ)),

    assert(currentPlayer(PlayerR)).

    



