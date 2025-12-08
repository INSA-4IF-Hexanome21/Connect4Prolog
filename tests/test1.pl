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
convertSymbol('r', '🔴').
convertSymbol('j', '🟡').
convert_symbol('e', '⬜').   
convertSymbol(X, X). 
convert_player(1, 'r').
convert_player(2, 'j').     


% Afficher le plateau
display_board :-
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
    column(Move,_,IndexMax),
    not(IndexMax == 6),
    !.

iaV2(Move,_) :-
    (
        (verifCol(Move),writeln("colonne"),!);
        (verifRow(Move),writeln("ligne"),!);
        (verifDiagLeft(Move),writeln("diagLeft"),!);
        (verifDiagRight(Move),writeln("diagRight"),!)
    ;
    (   
    	repeat,
        random(1,8,Move),
        column(Move,_,IndexMax),
        not(IndexMax == 6),
        !
    )
    ).

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
    column(Move,_,Row5),
    RowPlayed is  Row5 + 1,


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
    column(Move,_,Row5),
    RowPlayed is  Row5 + 1,

    % Vérifier que jouer dans cette colonne est possible
    canPlay(Move, RowPlayed),
    !.


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   CAS DE TEST   --------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

testIaV1() :- 
    % Création du tableau
    assert(column(1,['r','r','r','j','r','r'],6)),
    assert(column(2,['j','r','r','r','j','r'],6)),
    assert(column(3,['r','r','r','j','r','j'],6)),
    assert(column(4,['j','j','j','r','j','j'],6)),
    assert(column(5,['j','r','r','j','r','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['j','r','r','j','r','j'],6)),
    ia(Move,_),
    Move == 6.


testIaV2Col() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','e','e','e'],3)),
    assert(column(2,['j','r','r','r','j','r'],6)),
    assert(column(3,['r','r','r','j','r','j'],6)),
    assert(column(4,['j','j','j','r','j','j'],6)),
    assert(column(5,['j','r','r','j','r','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['j','r','r','j','r','j'],6)),
    iaV2(Move,_),
    Move == 1.

testIaV2Row() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','j','e','e'],4)),
    assert(column(2,['j','r','j','r','j','r'],6)),
    assert(column(3,['r','r','j','j','r','j'],6)),
    assert(column(4,['r','j','r','e','e','e'],3)),
    assert(column(5,['r','r','r','j','j','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['j','r','r','j','r','j'],6)),
    iaV2(Move,_),
    Move == 6.

testIaV2Row2() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','j','e','e'],4)),
    assert(column(2,['j','r','j','r','j','r'],6)),
    assert(column(3,['j','r','j','j','r','j'],6)),
    assert(column(4,['r','j','r','e','e','e'],3)),
    assert(column(5,['r','r','r','j','j','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['r','r','r','j','r','j'],6)),
    iaV2(Move,_),
    Move == 6.

testIaV2Diag() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','j','e','e'],4)),
    assert(column(2,['j','r','j','r','j','r'],6)),
    assert(column(3,['r','r','r','j','r','j'],6)),
    assert(column(4,['j','j','r','e','e','e'],3)),
    assert(column(5,['j','r','r','j','j','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['r','r','r','j','r','j'],6)),
    iaV2(Move,_),
    write(Move),
    Move == 4.

testIaV2Diag2() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','j','e','e'],4)),
    assert(column(2,['j','r','j','r','j','r'],6)),
    assert(column(3,['r','r','j','j','r','j'],6)),
    assert(column(4,['j','j','r','e','e','e'],3)),
    assert(column(5,['j','r','r','j','j','r'],6)),
    assert(column(6,['j','r','j','e','e','e'],3)),
    assert(column(7,['r','r','r','j','r','j'],6)),
    iaV2(Move,_),
    write(Move),
    Move == 4.

testIaV2Diag3() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','j','e','e'],4)),
    assert(column(2,['j','r','j','r','j','r'],6)),
    assert(column(3,['r','r','e','e','e','e'],2)),
    assert(column(4,['j','j','r','r','e','e'],3)),
    assert(column(5,['j','r','r','j','j','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['r','r','r','j','r','j'],6)),
    display_board,
    iaV2(Move,_),
    write(Move),
    Move == 3.

testIaV2() :- testIaV2Col(),
   testIaV2Row(),
   testIaV2Row2(),
   testIaV2Diag(),
   testIaV2Diag2(),
   testIaV2Diag3().

    


