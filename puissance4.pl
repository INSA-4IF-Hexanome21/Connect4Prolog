/* :- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(pce)).

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
    writeln('bouton cliqué'),
    reply_html_page(
        title('Action'),
        [
            h2('Merci !'),
            p('Le prédicat Prolog a bien été exécuté.'),
            a([href('/')],'Retour')
        ]).

 */    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------------   VARIABLES   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic column/3. % column(Col, ColData, LastPos)

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
convertSymbol('r', '🔴').
convertSymbol('j', '🟡').
convertSymbol('e', '.').   
convertSymbol(X, X).     


% Afficher le plateau
displayBoard :-
    nl,
    % On parcourt les lignes de 1 (bas) à 6 (haut)
    forall(between(1,6,Row),
        (
            % Pour chaque colonne de 1 à 7
            forall(between(1,7,Col),
                (
                    column(Col, ColData, _),
                    nth1(Row, ColData, Cell),
                    convertSymbol(Cell, Symbol),
                    write(Symbol), write(' ')
                )
            ),
            nl
        )
    ),
    write('1 2 3 4 5 6 7'), nl, nl.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   MISE A JOUR DU PLATEAU   ------------ 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

updateBoard(Col,NewCol, X) :- 
    retract(column(X,Col,_)),
    assert(column(X,NewCol,_)).

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
    write('Le joueur '), write(Player), writeln(' a remporté la partie !').

isOver(_) :-
    isTie(),
    writeln('Match nul !').

isTie() :-
    \+ (column(_, _, LastPos), LastPos =< 6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   CHANGER DE JOUEUR   ---------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nextPlayer('j','r').
nextPlayer('r','j').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   BOUCLE DE JEU   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play(Player) :- 
    write('New turn for:'), writeln(Player),
    ai(_, Player),
    %playMove,
    isOver(Player),
    %updateBoard(Col,NewCol, X),
    displayBoard.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   INITIALISATION   ------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
initBoard :-
    retractall(column(_,_,_)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,1))
    ),
    displayBoard.