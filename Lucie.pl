%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------------   IMPORTS   ---------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Module d'interface de l'IA
%:- consult('ai.pl').
% Les gars, essayez d'importer vos modules au lieu de travailler ici ;)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------------   IHM   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
%:- use_module(library(pce)).

% Point d’entrée : démarrer le serveur sur le port 8080
start :-
    http_server(http_dispatch, [port(8080)]).

% Associe l'URL "/" au prédicat index/1
:- http_handler(root(.), index, []).

% Associe l'URL "/click" au prédicat on_click/1
:- http_handler(root(click), on_click, []).

% Génère la page principale
index(_Request) :-
    reply_html_page(
        title('Mini interface Prolog'),
        [
            h1('Ma mini-interface Web'),
            p('Clique sur le bouton :'),
            form([action('/click'), method('GET')],
                 input([type(submit), value('Clique !')]))
        ]).

% Ce prédicat réagit au clic
on_click(_Request) :-
    reply_html_page(
        title('Action'),
        [
            h2('Merci !'),
            p('Le prédicat Prolog a bien été exécuté.'),
            a([href('/')],'Retour')
        ]).

:- module(swi_demo, []).

      :- initialization(run).

      run :-
        Button := document.createElement("button"),
        Button.innerHTML := "click",
	_ := document.getElementById("swi-demo").appendChild(Button),
      bind(Button, click, _Ev, test(Button)).

      test(Button) :-
        Text := Button.innerHTML,
        string_concat(Text, " and again", NewText),
        Button.innerHTML := NewText.
   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------------   VARIABLES   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic column/3. % column(Col, ColData, LastPos)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------   CONDITIONS DE VICTOIRE   ------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Versions optimisées

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

% Versions non optimisées

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
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------   AFFICHAGE DU PLATEAU   --------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------------   IA   -------------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Trouver le meilleur mouvement
% Faire un mouvement aléatoire
ai(Move, _) :- 
    assert(column(1,['j',''])),
    repeat,
    random(1,7,Move),
    column(Move, _, IndexMax),
    not(IndexMax == 6),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ------------------ JOUER UN COUP  --------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   FIN DE PARTIE   --------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Appeler avant de changer de joueur

isOver(Player, Column) :-
    (   horizontalVictory(Player, Column)
    ;   verticalVictory(Player, Column)
    ;   leftDiagonalVictory(Player, Column)
    ;   rightDiagonalVictory(Player, Column)
    ),
    !,
    write(Player), writeln(' has won the match !').

isOver(_, _) :-
    isTie(),
    writeln('It\'s a tie !').

isTie() :-
    \+ (column(_, _, LastPos), LastPos =< 6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   CHANGER DE JOUEUR   ---------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nextPlayer(player('RED', TR), player('YELLOW', TY)).
nextPlayer(player('YELLOW', TY), player('RED', TR)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   BOUCLE DE JEU   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play(Player) :-
    Player = player(Color,Type),
    write('New turn for: '), writeln(Color),

    (Type == 'ai' -> 
        ai(Move,Player) %Appel l'IA pour un mouvement
    ;
        nl%to implement
    ),   

    displayBoard,
    playMove(Move, Color, NewCol),   

    (   isOver(Color, Move) -> 
        true % on stoppe le jeu
    ;
        nextPlayer(Player,NextPlayer),
        play(NextPlayer) % on continue de jouer
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
    initPlay(PlayerR, PlayerJ),
    displayBoard.

initPlayer(PlayerR, PlayerJ) :-
    writeln('--- Red Player (RED) ---'),
    selectPlayerType(TypeR),
    nl, write('Red Player is '), writeln(TypeR),

    writeln('--- Yellow Player (YELLOW) ---'),
    selectPlayerType(TypeJ),
    nl, write('Yellow Player is '), writeln(TypeJ),

    PlayerR = player('RED',TypeR),
    PlayerJ = player('YELLOW',TypeJ).

initPlay(PlayerR, PlayerJ):- 
    play(PlayerR).