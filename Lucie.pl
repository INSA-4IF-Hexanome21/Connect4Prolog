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

:- dynamic column/3. %column(Col, ColData,LastPos)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------   CONDITIONS DE VICTOIRE   ------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%Detecter Victoire
%Detecter Victoire Horizontale
%Detecter Victoire Verticale
%Detecter Victoire Diagonale ↖
%Detecter Victoire Diagonale ↗

% En cours en bas…
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------   AFFICHAGE DU PLATEAU   --------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_symbol('r', '🔴').
convert_symbol('j', '🟡').
convert_symbol('e', '.').   
convert_symbol(X, X).     


%Afficher Le Plateau
display_board :-
    nl,
    % On parcourt les lignes de 1 (bas) à 6 (haut)
    forall(between(1,6,Row),
        (
            % Pour chaque colonne de 1 à 7
            forall(between(1,7,Col),
                (
                    column(Col, ColData,LastPos),
                    nth1(Row, ColData, Cell),
                    convert_symbol(Cell, Symbol),
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

%Trouver Le Meilleur Mouvement
%Faire Un Mouvement Aleatoire
ia(Move,_) :- 
    assert(column(1,['j','r','r','j','r','j'],6)),
    assert(column(2,['j','r','r','e','e','e'],3)),
    assert(column(3,['j','r','r','j','e','e'],4)),
    assert(column(4,['j','r','r','j','r','e'],5)),
    assert(column(5,['j','r','e','e','e','e'],2)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['j','r','r','j','r','j'],6)),
    repeat,
    random(1,8,Move),
    column(Move,_,IndexMax),
    not(IndexMax == 6),
    !.

iaV2(Move,_) :-
    assert(column(1,['j','r','r','j','r','j'],6)),
    assert(column(2,['j','r','r','e','e','e'],3)),
    assert(column(3,['j','r','r','j','e','e'],4)),
    assert(column(4,['j','r','r','r','e','e'],5)),
    assert(column(5,['j','r','e','e','e','e'],2)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['j','r','r','j','r','j'],6)),
    repeat,

    (
        (verifCol(Move),!);
        (verifRow(Move),!)
    ),
    random(1,8,Move),
    column(Move,_,IndexMax),
    not(IndexMax == 6),
    !.

verifCol(Move).

    
VerifRow(Move) :-
    between(1, 6, Row),
    between(1, 4, StartCol),
    EndCol is StartCol + 2,
    forall(between(StartCol, EndCol, Col),
        (
            column(Col, ColData, _),
            nth1(Row, ColData, X)
        )
    ).
VerifRow(Move) :-
    forall(between(1,6,Row),
        (
            column(Col, ColData, LastPos),
            nth1(Row, ColData, Cell),
            convert_symbol(Cell, Symbol),
            write(Symbol), write(' ')
        )
    ),


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ------------------ JOUER UN COUP  --------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Creer Un Nouveau Plateau Avec Le Nouveau Mouv

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   MISE A JOUR DU PLATEAU   ------------ 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Mettre A Jour Le Plateau Original
maj_plateau(Col,NewCol, X) :- 
    retract(column(X,Col,_)),
    assert(column(X,NewCol,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   FIN DE PARTIE   --------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Detecter Si Victoire Ou Egalite Et Afficher Gagnant
% Detecter Si Le Joueur Courrant A Gagner
% Dtetecter Si Le Joueur Ennemi A Gagner
% Detecter Une Egalite

% Appeler avant de changer de joueur

detecter_fin(Player) :-
    (   victoire_horizontale(Player)
    ;   victoire_verticale(Player)
    ;   victoire_diagonale_gauche(Player)
    ;   victoire_diagonale_droite(Player)
    ;   match_nul()
    ).

victoire_horizontale(Player) :-
    between(1, 6, Row),
    between(1, 4, StartCol),
    EndCol is StartCol + 3,
    forall(between(StartCol, EndCol, Col),
        (
            column(Col, ColData, _),
            nth1(Row, ColData, Player)
        )
    ).

victoire_verticale(Player) :-
    between(1, 7, Col),
    column(Col, ColData, _),
    between(1, 3, StartRow),
    EndRow is StartRow + 3,
    forall(between(StartRow, EndRow, Row),
        (
            nth1(Row, ColData, Player)
        )
    ).

victoire_diagonale_gauche(Player) :-
    between(1, 3, StartRow),
    between(1, 4, StartCol),
    EndRow is StartRow + 3,
    EndCol is StartCol + 3,
    forall(between(0, 3, Offset),
        (
            Row is StartRow + Offset,
            Col is StartCol + Offset,
            column(Col, ColData, _),
            nth1(Row, ColData, Player)
        )
    ).

victoire_diagonale_droite(Player).

match_nul().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   CHANGER DE JOUEUR   ---------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nextPlayer('j','r').
nextPlayer('r','j').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   BOUCLE DE JEU   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jouer(Player) :- detecter_fin(Player).

jouer(Player) :- 
    write('New turn for:'), writeln(Player),
    ia(Move,Player),
    %playMpve
    %%maj_plateau(Col,NewCol, X),
    nextPlayer(Player,NextPlayer),
    display_board,
    jouer(NextPlayer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   INITIALISATION   ------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
init_board :-
    retractall(column(_,_,_)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,1))
    ),
    display_board.



