%CETAIT LE DERNIER

:- dynamic compteur_noeuds/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   CONFIGURATION   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

largeur_plateau(7).
hauteur_plateau(6).

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

% FIX: Vérifier que Compte est bien un nombre avant de comparer
diagonale_montante(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    between(0, L, ColDebut),
    ColDebut < L,
    between(0, H, LigneDebut),
    LigneDebut < H,
    verifier_diagonale(Plateau, ColDebut, LigneDebut, 1, 1, Jeton, Compte),
    number(Compte),  % S'assurer que c'est un nombre
    Compte >= 4.

diagonale_descendante(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    between(0, L, ColDebut),
    ColDebut < L,
    between(0, H, LigneDebut),
    LigneDebut < H,
    verifier_diagonale(Plateau, ColDebut, LigneDebut, 1, -1, Jeton, Compte),
    number(Compte),  % S'assurer que c'est un nombre
    Compte >= 4.

verifier_diagonale(Plateau, Col, Ligne, DeltaCol, DeltaLigne, Jeton, Compte) :-
    verifier_diagonale_aux(Plateau, Col, Ligne, DeltaCol, DeltaLigne, Jeton, 0, Compte).

% FIX: Gérer tous les cas proprement
verifier_diagonale_aux(Plateau, Col, Ligne, _, _, _, CompteAcc, CompteAcc) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    (   Col >= L
    ;   Col < 0
    ;   Ligne >= H
    ;   Ligne < 0
    ), !.  % Hors limites, retourner le compte accumulé

verifier_diagonale_aux(Plateau, Col, Ligne, DeltaCol, DeltaLigne, Jeton, 
                       CompteAcc, Compte) :-
    % Vérifier si la colonne existe et a assez d'éléments
    (   nth0(Col, Plateau, Colonne),
        length(Colonne, Longueur),
        Longueur > Ligne,
        nth0(Ligne, Colonne, ValeurCellule),
        ValeurCellule == Jeton
    ->  % Jeton trouvé, continuer
        NouveauCompte is CompteAcc + 1,
        NouvelleCol is Col + DeltaCol,
        NouvelleLigne is Ligne + DeltaLigne,
        verifier_diagonale_aux(Plateau, NouvelleCol, NouvelleLigne, 
                               DeltaCol, DeltaLigne, Jeton, NouveauCompte, Compte)
    ;   % Pas le bon jeton ou case vide, arrêter
        Compte = CompteAcc
    ), !.

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
%%%% -------   FONCTION D'EVALUATION HEURISTIQUE   -------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Évalue une position (pas de victoire/défaite immédiate)
evaluer_position(Plateau, Jeton, Score) :-
    adversaire(Jeton, Adversaire),
    
    % Compter les menaces (3 en ligne avec espace vide)
    compter_menaces(Plateau, Jeton, MenacesJoueur),
    compter_menaces(Plateau, Adversaire, MenacesAdversaire),
    
    % Bonus pour colonnes centrales (colonnes 2,3,4 valent plus)
    compter_centre(Plateau, Jeton, CentreJoueur),
    compter_centre(Plateau, Adversaire, CentreAdversaire),
    
    % Calcul du score heuristique
    Score is (MenacesJoueur * 50) - (MenacesAdversaire * 50) + 
             (CentreJoueur * 3) - (CentreAdversaire * 3).

% Compte le nombre de "presque 4" (3 jetons alignés)
compter_menaces(Plateau, Jeton, Menaces) :-
    findall(1, menace_trouvee(Plateau, Jeton), Liste),
    length(Liste, Menaces).

menace_trouvee(Plateau, Jeton) :-
    (   menace_horizontale(Plateau, Jeton)
    ;   menace_verticale(Plateau, Jeton)
    ;   menace_diagonale(Plateau, Jeton)
    ).

menace_horizontale(Plateau, Jeton) :-
    hauteur_plateau(H),
    between(0, H, Ligne),
    Ligne < H,
    ligne_complete(Plateau, Ligne, ListeLigne),
    sous_liste_consecutive(ListeLigne, Jeton, 3).

menace_verticale(Plateau, Jeton) :-
    member(Colonne, Plateau),
    sous_liste_consecutive(Colonne, Jeton, 3).

% FIX: Même correction pour les menaces diagonales
menace_diagonale(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    between(0, L, ColDebut),
    ColDebut < L,
    between(0, H, LigneDebut),
    LigneDebut < H,
    verifier_diagonale(Plateau, ColDebut, LigneDebut, 1, 1, Jeton, C1),
    number(C1),
    C1 >= 3.

menace_diagonale(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    between(0, L, ColDebut),
    ColDebut < L,
    between(0, H, LigneDebut),
    LigneDebut < H,
    verifier_diagonale(Plateau, ColDebut, LigneDebut, 1, -1, Jeton, C2),
    number(C2),
    C2 >= 3.

% Compte les jetons dans les colonnes centrales (2, 3, 4)
compter_centre(Plateau, Jeton, Total) :-
    (   nth0(2, Plateau, Col2) -> true ; Col2 = []),
    (   nth0(3, Plateau, Col3) -> true ; Col3 = []),
    (   nth0(4, Plateau, Col4) -> true ; Col4 = []),
    findall(1, (member(Col, [Col2, Col3, Col4]), member(Jeton, Col)), Liste),
    length(Liste, Total).

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
%%%% ------------ ALGORITHME NEGAMAX AMELIORE ------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

incrementer_compteur :-
    (   retract(compteur_noeuds(N))
    ->  N1 is N + 1,
        assertz(compteur_noeuds(N1))
    ;   assertz(compteur_noeuds(1))
    ).

% CAS 1: Profondeur 0 → EVALUER avec heuristique
negamax(Plateau, Jeton, 0, _, _, Score) :- 
    !,
    evaluer_position(Plateau, Jeton, Score).

% CAS 2: Profondeur > 0 → Recherche
negamax(Plateau, Jeton, Prof, Alpha, Beta, Score) :-
    incrementer_compteur,
    coups_possibles(Plateau, CoupsPossibles),
    
    % Victoire immédiate ?
    (   member(Col, CoupsPossibles),
        coup_gagnant(Plateau, Col, Jeton)
    ->  Score is 10000 - Prof  % Victoire rapide vaut plus
    ;   Prof > 0
    ->  negamax_recherche(Plateau, Jeton, Prof, Alpha, Beta, Score)
    ;   evaluer_position(Plateau, Jeton, Score)
    ).

negamax_recherche(Plateau, Jeton, Prof, Alpha, Beta, Score) :-
    adversaire(Jeton, Adversaire),
    coups_non_perdants(Plateau, Jeton, Coups),
    (   Coups = []
    ->  Score is -9000  % Défaite forcée
    ;   NouvelleProf is Prof - 1,
        % IMPORTANT: Ordre des coups - centre d'abord
        trier_coups_centre(Coups, CoupsTriés),
        negamax_explorer_coups(Plateau, CoupsTriés, Jeton, Adversaire, 
                               NouvelleProf, Alpha, Beta, Score)
    ).

% Trie les coups pour mettre les colonnes centrales en premier
trier_coups_centre(Coups, CoupsTriés) :-
    sort_by_key(Coups, priorite_coup, CoupsTriés).

sort_by_key(Liste, _, Liste).  % Version simple (pas de tri pour l'instant)

priorite_coup(3, 10) :- !.  % Colonne centrale (3) = priorité max
priorite_coup(2, 8) :- !.
priorite_coup(4, 8) :- !.
priorite_coup(1, 5) :- !.
priorite_coup(5, 5) :- !.
priorite_coup(_, 3).

negamax_explorer_coups(_, [], _, _, _, Alpha, _, Alpha).
negamax_explorer_coups(Plateau, [Col|Reste], Jeton, Adversaire, 
                       Prof, Alpha, Beta, Score) :-
    jouer_coup(Plateau, Col, Jeton, NouveauPlateau),
    BetaNeg is -Beta,
    AlphaNeg is -Alpha,
    negamax(NouveauPlateau, Adversaire, Prof, BetaNeg, AlphaNeg, ScoreNeg),
    ScoreCoup is -ScoreNeg,
    (   ScoreCoup >= Beta
    ->  Score = ScoreCoup  % Coupure Beta
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

profundidad_maxima(6).  % Augmenté de 4 à 6 pour meilleur jeu

resoudre(Plateau, Jeton, Prof, Score) :-
    negamax(Plateau, Jeton, Prof, -100000, 100000, Score).

analyser(Plateau, Jeton, Scores) :-
    retractall(compteur_noeuds(_)),
    assertz(compteur_noeuds(0)),
    largeur_plateau(L),
    profundidad_maxima(ProfMax),
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
    ->  Score is 10000  % Victoire immédiate
    ;   jouer_coup(Plateau, Col, Jeton, NouveauPlateau),
        adversaire(Jeton, Adversaire),
        ProfSuivante is Prof - 1,
        resoudre(NouveauPlateau, Adversaire, ProfSuivante, ScoreAdv),
        Score is -ScoreAdv
    ).