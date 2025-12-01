%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------------   VARIABLES   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic column/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------   CONDITIONS DE VICTOIRE   ------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%Detecter Victoire
%Detecter Victoire Horizontale
%Detecter Victoire Verticale
%Detecter Victoire Diagonale ↖
%Detecter Victoire Diagonale ↗
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------   AFFICHAGE DU PLATEAU   --------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_symbol('r', '🔴').
convert_symbol('j', '🟡').
convert_symbol('e', '⬜').   
convert_symbol(X, X).   

%Afficher Le Plateau
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
    column(Move,Col,IndexMax),
    not(IndexMax == 6),
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

% Detecter Si Victoire Ou Egalite Et Afficher Gagnant
% Detecter Si Le Joueur Courrant A Gangner
% Dtetecter Si Le Joueur Enemie A Ganer
% Detecter Une Egalite

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
    ia(Move,Player),                    %Appel l'IA pour un mouvement
    playMove(Move, Player, NewCol),   
    nextPlayer(Player,NextPlayer),
    display_board,
    play(NextPlayer).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   INITIALISATION   ------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
init_board :-
    retractall(column(_,_,_)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    display_board,
    play('r').



