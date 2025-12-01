%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------------   VARIABLES   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic column/3. % column(Col, ColData, LastPos)
num_rows(6).
num_cols(7).

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
convert_player(1, 'r').
convert_player(2, 'j').    


%Afficher Le Plateau
displayBoard :-
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
    % assert(column(1,['j','r','r','j','r','j']),6),
    % assert(column(1,['j','r','r',A,B,C]),3),
    % assert(column(1,['j','r','r','j',D,E]),4),
    % assert(column(1,['j','r','r','j','r',F]),5),
    % assert(column(1,['j','r',G,H,I,J]),2),
    % assert(column(1,[K,L,M,N,O,P]),0),
    repeat,
    random(1,7,Move),
    column(Move,_,IndexMax),
    not(IndexMax == 6),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ------------------ JOUER UN COUP  --------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%play_move(Move,Player) :-
%    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   MISE A JOUR DU PLATEAU   ------------ 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Mettre A Jour Le Plateau Original
applyIt(Col,NewCol, X) :- 
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

isEndgame(Player) :-
    (
        victoire_horizontale(Player);
        victoire_verticale(Player);
        victoire_diagonale_gauche(Player);
        victoire_diagonale_droite(Player);
        match_nul()
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

play(Player) :- detecter_fin(Player).

play(Player) :- 
    write('New turn for:'), writeln(Player),
    ia(Move,Player),                    %Appel l'IA pour un mouvement
    %playMpve                           %réalise le mouvement 
    %%mapplyIt(Col,NewCol, X),       
    isEndGame(Player),
    nextPlayer(Player,NextPlayer),
    displayBoard,
    jouer(NextPlayer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   INITIALISATION   ------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
init_board :-
    retractall(column(_,_,_)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    displayBoard,
    random(1,3,Number),
    writeln(Number),
    convert_player(Number, Player),
    write('First Player: '), writeln(Player).
    %play(Player).
    



