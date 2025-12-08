:- dynamic compteur_noeuds/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   CONFIGURATION   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

largeur_plateau(7).
hauteur_plateau(6).

score_minimum(Score) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    Score is -(L * H) // 2.

score_maximum(Score) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    Score is (L * H + 1) // 2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   PREDICATS DE BASE   ---------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adversaire(x, o).
adversaire(o, x).

colonne_jouable(Colonne, Hauteur) :-
    length(Colonne, Longueur),
    Longueur < Hauteur.

nombre_coups(Plateau, NbCoups) :-
    findall(Jeton, (member(Col, Plateau), member(Jeton, Col)), Jetons),
    length(Jetons, NbCoups).

joueur_actuel(Plateau, Jeton) :-
    nombre_coups(Plateau, NbCoups),
    (   NbCoups mod 2 =:= 0
    ->  Jeton = x
    ;   Jeton = o
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   GESTION DES COUPS   ---------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

coups_possibles(Plateau, Coups) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    findall(Col,
            (between(0, L, Col),
             Col < L,
             nth0(Col, Plateau, Colonne),
             colonne_jouable(Colonne, H)),
            Coups).

jouer_coup(Plateau, Col, Jeton, NouveauPlateau) :-
    nth0(Col, Plateau, Colonne, Reste),
    append(Colonne, [Jeton], NouvelleColonne),
    nth0(Col, NouveauPlateau, NouvelleColonne, Reste).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   DETECTION VICTOIRE   --------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

coup_gagnant(Plateau, Col, Jeton) :-
    jouer_coup(Plateau, Col, Jeton, NouveauPlateau),
    position_gagnante(NouveauPlateau, Jeton).

position_gagnante(Plateau, Jeton) :-
    (   victoire_horizontale(Plateau, Jeton)
    ;   victoire_verticale(Plateau, Jeton)
    ;   victoire_diagonale(Plateau, Jeton)
    ).

victoire_horizontale(Plateau, Jeton) :-
    hauteur_plateau(H),
    between(0, H, Ligne),
    Ligne < H,
    ligne_complete(Plateau, Ligne, ListeLigne),
    sous_liste_consecutive(ListeLigne, Jeton, 4).

ligne_complete(Plateau, NumLigne, Ligne) :-
    findall(J,
            (member(Col, Plateau),
             length(Col, Longueur),
             Longueur > NumLigne,
             nth0(NumLigne, Col, J)),
            Ligne).

victoire_verticale(Plateau, Jeton) :-
    member(Colonne, Plateau),
    sous_liste_consecutive(Colonne, Jeton, 4).

victoire_diagonale(Plateau, Jeton) :-
    (   diagonale_montante(Plateau, Jeton)
    ;   diagonale_descendante(Plateau, Jeton)
    ).

diagonale_montante(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    between(0, L, ColDebut),
    ColDebut < L,
    between(0, H, LigneDebut),
    LigneDebut < H,
    verifier_diagonale(Plateau, ColDebut, LigneDebut, 1, 1, Jeton, 
                       Compte),
    Compte >= 4.

diagonale_descendante(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    between(0, L, ColDebut),
    ColDebut < L,
    between(0, H, LigneDebut),
    LigneDebut < H,
    verifier_diagonale(Plateau, ColDebut, LigneDebut, 1, -1, Jeton, 
                       Compte),
    Compte >= 4.

verifier_diagonale(Plateau, Col, Ligne, DeltaCol, DeltaLigne, Jeton, 
                   Compte) :-
    verifier_diagonale_aux(Plateau, Col, Ligne, DeltaCol, DeltaLigne, 
                           Jeton, 0, Compte).

verifier_diagonale_aux(Plateau, Col, Ligne, DeltaCol, DeltaLigne, Jeton, 
                       CompteAcc, Compte) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    (   Col >= L
    ->  Compte = CompteAcc
    ;   Col < 0
    ->  Compte = CompteAcc
    ;   Ligne >= H
    ->  Compte = CompteAcc
    ;   Ligne < 0
    ->  Compte = CompteAcc
    ;   nth0(Col, Plateau, Colonne),
        length(Colonne, Longueur),
        Longueur > Ligne,
        nth0(Ligne, Colonne, Jeton)
    ->  NouveauCompte is CompteAcc + 1,
        NouvelleCol is Col + DeltaCol,
        NouvelleLigne is Ligne + DeltaLigne,
        verifier_diagonale_aux(Plateau, NouvelleCol, NouvelleLigne, 
                               DeltaCol, DeltaLigne, Jeton, NouveauCompte, 
                               Compte)
    ;   Compte = CompteAcc
    ).

sous_liste_consecutive(Liste, Element, N) :-
    append(_, Reste, Liste),
    longueur_prefixe(Reste, Element, Longueur),
    Longueur >= N.

longueur_prefixe([Element|Reste], Element, Longueur) :-
    !,
    longueur_prefixe(Reste, Element, Longueur1),
    Longueur is Longueur1 + 1.
longueur_prefixe(_, _, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   COUPS NON PERDANTS   --------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

coups_non_perdants(Plateau, Jeton, Coups) :-
    adversaire(Jeton, Adversaire),
    coups_possibles(Plateau, TousCoups),
    findall(Col,
            (member(Col, TousCoups),
             \+ coup_gagnant(Plateau, Col, Adversaire)),
            Coups).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ------------ ALGORITHME NEGAMAX  -------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

incrementer_compteur :-
    (   retract(compteur_noeuds(N))
    ->  N1 is N + 1,
        assertz(compteur_noeuds(N1))
    ;   assertz(compteur_noeuds(1))
    ).

% --- COUT PROFONDEUR ---
negamax(_, _, 0, _, _, 0) :- !.

negamax(Plateau, Jeton, Prof, Alpha, Beta, Score) :-
    incrementer_compteur,
    coups_possibles(Plateau, CoupsPossibles),
    (   member(Col, CoupsPossibles),
        coup_gagnant(Plateau, Col, Jeton)
    -> 
       
        Score is 1000 + Prof
    ;   Prof > 0
    -> 
        negamax_recherche(Plateau, Jeton, Prof, Alpha, Beta, Score)
    ;  
        Score = 0
    ).

negamax_recherche(Plateau, Jeton, Prof, Alpha, Beta, Score) :-
    adversaire(Jeton, Adversaire),
    coups_non_perdants(Plateau, Jeton, Coups),
    (   Coups = []
    ->  % No moves
        Score = 0
    ;   NouvelleProf is Prof - 1,
        negamax_explorer_coups(Plateau, Coups, Jeton, Adversaire, 
                               NouvelleProf, Alpha, Beta, Score)
    ).

negamax_explorer_coups(_, [], _, _, _, Alpha, _, Alpha).
negamax_explorer_coups(Plateau, [Col|Reste], Jeton, Adversaire, 
                       Prof, Alpha, Beta, Score) :-
    jouer_coup(Plateau, Col, Jeton, NouveauPlateau),
    BetaNeg is -Beta,
    AlphaNeg is -Alpha,
    negamax(NouveauPlateau, Adversaire, Prof, BetaNeg, AlphaNeg, ScoreNeg),
    ScoreCoup is -ScoreNeg,
    (   ScoreCoup >= Beta
    ->  Score = ScoreCoup
    ;   ScoreCoup > Alpha
    ->  NouvelAlpha = ScoreCoup,
        negamax_explorer_coups(Plateau, Reste, Jeton, Adversaire, 
                               Prof, NouvelAlpha, Beta, Score)
    ;   negamax_explorer_coups(Plateau, Reste, Jeton, Adversaire, 
                               Prof, Alpha, Beta, Score)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   SOLVER PRINCIPAL   ----------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% depth IA (4 normal)
profundidad_maxima(4).

resoudre(Plateau, Jeton, Prof, Score) :-
    score_minimum(Min),
    score_maximum(Max),
    negamax(Plateau, Jeton, Prof, Min, Max, Score).

analyser(Plateau, Jeton, Scores) :-
    retractall(compteur_noeuds(_)),
    assertz(compteur_noeuds(0)),
    largeur_plateau(L),
    profundidad_maxima(ProfMax), % límite
    findall(Score,
            (between(0, L, Col),
             Col < L,
             analyser_coup(Plateau, Col, Jeton, ProfMax, Score)),
            Scores).

analyser_coup(Plateau, Col, Jeton, Prof, Score) :-
    largeur_plateau(L),
    (   Col >= L
    ->  Score = invalide
    ;   \+ nth0(Col, Plateau, Colonne)
    ->  Score = invalide
    ;   hauteur_plateau(H),
        \+ colonne_jouable(Colonne, H)
    ->  Score = invalide
    ;   coup_gagnant(Plateau, Col, Jeton)
    ->  Score is 1000 + Prof % Victoria inmediata
    ;   jouer_coup(Plateau, Col, Jeton, NouveauPlateau),
        adversaire(Jeton, Adversaire),
        ProfSuivante is Prof - 1,
        resoudre(NouveauPlateau, Adversaire, ProfSuivante, ScoreAdv),
        Score is -ScoreAdv
    ).