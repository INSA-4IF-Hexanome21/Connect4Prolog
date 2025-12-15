:- dynamic compteur_noeuds/1.

:- use_module(library(lists)).
:- use_module(library(pairs)).

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
%%%% ---------------   ACCES CASES (robuste)   ------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% valeur_case/4 renvoie x/o si présent, sinon vide.
valeur_case(Plateau, Col, Ligne, Valeur) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    Col >= 0,
    Col < L,
    Ligne >= 0,
    Ligne < H,
    nth0(Col, Plateau, Colonne),
    length(Colonne, Longueur),
    (   Longueur > Ligne
    ->  nth0(Ligne, Colonne, Valeur)
    ;   Valeur = vide
    ).

% Une case vide est jouable si c'est exactement la prochaine hauteur libre.
case_jouable(Plateau, Col, Ligne) :-
    hauteur_plateau(H),
    Ligne >= 0,
    Ligne < H,
    nth0(Col, Plateau, Colonne),
    length(Colonne, Longueur),
    Ligne =:= Longueur.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   GESTION DES COUPS   ----------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

coups_possibles(Plateau, Coups) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    findall(Col,
            (between(0, L, Col), Col < L, nth0(Col, Plateau, Colonne),
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

% Fenêtres de 4 en horizontal
victoire_horizontale(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    between(0, H, Ligne),
    Ligne < H,
    MaxColDebut is L - 4,
    between(0, MaxColDebut, ColDebut),
    valeur_case(Plateau, ColDebut, Ligne, Jeton),
    Col1 is ColDebut + 1,
    Col2 is ColDebut + 2,
    Col3 is ColDebut + 3,
    valeur_case(Plateau, Col1, Ligne, Jeton),
    valeur_case(Plateau, Col2, Ligne, Jeton),
    valeur_case(Plateau, Col3, Ligne, Jeton).

% Fenêtres de 4 en vertical
victoire_verticale(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    between(0, L, Col),
    Col < L,
    MaxLigneDebut is H - 4,
    between(0, MaxLigneDebut, LigneDebut),
    valeur_case(Plateau, Col, LigneDebut, Jeton),
    L1 is LigneDebut + 1,
    L2 is LigneDebut + 2,
    L3 is LigneDebut + 3,
    valeur_case(Plateau, Col, L1, Jeton),
    valeur_case(Plateau, Col, L2, Jeton),
    valeur_case(Plateau, Col, L3, Jeton).

victoire_diagonale(Plateau, Jeton) :-
    (   diagonale_montante(Plateau, Jeton)
    ;   diagonale_descendante(Plateau, Jeton)
    ).

% Diagonale montante ( +1 col, +1 ligne )
diagonale_montante(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    MaxColDebut is L - 4,
    MaxLigneDebut is H - 4,
    between(0, MaxColDebut, ColDebut),
    between(0, MaxLigneDebut, LigneDebut),
    valeur_case(Plateau, ColDebut, LigneDebut, Jeton),
    C1 is ColDebut + 1,
    C2 is ColDebut + 2,
    C3 is ColDebut + 3,
    L1 is LigneDebut + 1,
    L2 is LigneDebut + 2,
    L3 is LigneDebut + 3,
    valeur_case(Plateau, C1, L1, Jeton),
    valeur_case(Plateau, C2, L2, Jeton),
    valeur_case(Plateau, C3, L3, Jeton).

% Diagonale descendante ( +1 col, -1 ligne )
diagonale_descendante(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    MaxColDebut is L - 4,
    between(0, MaxColDebut, ColDebut),
    between(3, H, LigneDebut),
    LigneDebut < H,
    valeur_case(Plateau, ColDebut, LigneDebut, Jeton),
    C1 is ColDebut + 1,
    C2 is ColDebut + 2,
    C3 is ColDebut + 3,
    L1 is LigneDebut - 1,
    L2 is LigneDebut - 2,
    L3 is LigneDebut - 3,
    valeur_case(Plateau, C1, L1, Jeton),
    valeur_case(Plateau, C2, L2, Jeton),
    valeur_case(Plateau, C3, L3, Jeton).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% -------   FONCTION D'EVALUATION HEURISTIQUE   --------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluer_position(Plateau, Jeton, Score) :-
    adversaire(Jeton, Adversaire),
    compter_menaces(Plateau, Jeton, MenacesJoueur),
    compter_menaces(Plateau, Adversaire, MenacesAdversaire),
    compter_centre(Plateau, Jeton, CentreJoueur),
    compter_centre(Plateau, Adversaire, CentreAdversaire),
    Score is (MenacesJoueur * 80) - (MenacesAdversaire * 90) +
             (CentreJoueur * 3) - (CentreAdversaire * 3).

% Menace = une fenêtre de 4 avec exactement 3 Jeton + 1 vide,
% et la case vide est jouable (gravité).
compter_menaces(Plateau, Jeton, Menaces) :-
    findall(1, menace_trouvee(Plateau, Jeton), Liste),
    length(Liste, Menaces).

menace_trouvee(Plateau, Jeton) :-
    menace_fenetre_horizontale(Plateau, Jeton).
menace_trouvee(Plateau, Jeton) :-
    menace_fenetre_verticale(Plateau, Jeton).
menace_trouvee(Plateau, Jeton) :-
    menace_fenetre_diag_montante(Plateau, Jeton).
menace_trouvee(Plateau, Jeton) :-
    menace_fenetre_diag_descendante(Plateau, Jeton).

menace_fenetre_horizontale(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    between(0, H, Ligne),
    Ligne < H,
    MaxColDebut is L - 4,
    between(0, MaxColDebut, ColDebut),
    C1 is ColDebut + 1,
    C2 is ColDebut + 2,
    C3 is ColDebut + 3,
    valeur_case(Plateau, ColDebut, Ligne, V0),
    valeur_case(Plateau, C1, Ligne, V1),
    valeur_case(Plateau, C2, Ligne, V2),
    valeur_case(Plateau, C3, Ligne, V3),
    exactement_trois_et_un_vide([V0, V1, V2, V3], Jeton, IndexVide),
    nth0(IndexVide, [ColDebut, C1, C2, C3], ColVide),
    case_jouable(Plateau, ColVide, Ligne).

menace_fenetre_verticale(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    between(0, L, Col),
    Col < L,
    MaxLigneDebut is H - 4,
    between(0, MaxLigneDebut, LigneDebut),
    L1 is LigneDebut + 1,
    L2 is LigneDebut + 2,
    L3 is LigneDebut + 3,
    valeur_case(Plateau, Col, LigneDebut, V0),
    valeur_case(Plateau, Col, L1, V1),
    valeur_case(Plateau, Col, L2, V2),
    valeur_case(Plateau, Col, L3, V3),
    exactement_trois_et_un_vide([V0, V1, V2, V3], Jeton, IndexVide),
    nth0(IndexVide, [LigneDebut, L1, L2, L3], LigneVide),
    case_jouable(Plateau, Col, LigneVide).

menace_fenetre_diag_montante(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    MaxColDebut is L - 4,
    MaxLigneDebut is H - 4,
    between(0, MaxColDebut, ColDebut),
    between(0, MaxLigneDebut, LigneDebut),
    C1 is ColDebut + 1,
    C2 is ColDebut + 2,
    C3 is ColDebut + 3,
    L1 is LigneDebut + 1,
    L2 is LigneDebut + 2,
    L3 is LigneDebut + 3,
    valeur_case(Plateau, ColDebut, LigneDebut, V0),
    valeur_case(Plateau, C1, L1, V1),
    valeur_case(Plateau, C2, L2, V2),
    valeur_case(Plateau, C3, L3, V3),
    exactement_trois_et_un_vide([V0, V1, V2, V3], Jeton, IndexVide),
    nth0(IndexVide, [ColDebut, C1, C2, C3], ColVide),
    nth0(IndexVide, [LigneDebut, L1, L2, L3], LigneVide),
    case_jouable(Plateau, ColVide, LigneVide).

menace_fenetre_diag_descendante(Plateau, Jeton) :-
    largeur_plateau(L),
    hauteur_plateau(H),
    MaxColDebut is L - 4,
    between(0, MaxColDebut, ColDebut),
    between(3, H, LigneDebut),
    LigneDebut < H,
    C1 is ColDebut + 1,
    C2 is ColDebut + 2,
    C3 is ColDebut + 3,
    L1 is LigneDebut - 1,
    L2 is LigneDebut - 2,
    L3 is LigneDebut - 3,
    valeur_case(Plateau, ColDebut, LigneDebut, V0),
    valeur_case(Plateau, C1, L1, V1),
    valeur_case(Plateau, C2, L2, V2),
    valeur_case(Plateau, C3, L3, V3),
    exactement_trois_et_un_vide([V0, V1, V2, V3], Jeton, IndexVide),
    nth0(IndexVide, [ColDebut, C1, C2, C3], ColVide),
    nth0(IndexVide, [LigneDebut, L1, L2, L3], LigneVide),
    case_jouable(Plateau, ColVide, LigneVide).

exactement_trois_et_un_vide(Liste, Jeton, IndexVide) :-
    compter_occurrences(Liste, Jeton, NJ),
    compter_occurrences(Liste, vide, NV),
    NJ =:= 3,
    NV =:= 1,
    nth0(IndexVide, Liste, vide).

compter_occurrences([], _, 0).
compter_occurrences([X|Reste], X, N) :-
    !,
    compter_occurrences(Reste, X, N1),
    N is N1 + 1.
compter_occurrences([_|Reste], X, N) :-
    compter_occurrences(Reste, X, N).

% Colonnes centrales (index 2,3,4 en 0-based)
compter_centre(Plateau, Jeton, Total) :-
    (nth0(2, Plateau, Col2) -> true ; Col2 = []),
    (nth0(3, Plateau, Col3) -> true ; Col3 = []),
    (nth0(4, Plateau, Col4) -> true ; Col4 = []),
    findall(1, (member(Col, [Col2, Col3, Col4]), member(Jeton, Col)), Liste),
    length(Liste, Total).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   COUPS NON PERDANTS   ---------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fix ioi,  on filtre les coups qui APRES notre coup donnent
% un coup gagnant immédiat à l'adversaire (dans n'importe quelle colonne).
coups_non_perdants(Plateau, Jeton, Coups) :-
    adversaire(Jeton, Adversaire),
    coups_possibles(Plateau, TousCoups),
    findall(Col,
            (member(Col, TousCoups),
             jouer_coup(Plateau, Col, Jeton, NouveauPlateau),
             \+ existe_coup_gagnant(NouveauPlateau, Adversaire)),
            Coups).

existe_coup_gagnant(Plateau, Jeton) :-
    coups_possibles(Plateau, Coups),
    member(Col, Coups),
    coup_gagnant(Plateau, Col, Jeton),
    !.

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
negamax(Plateau, Jeton, Prof, _, _, Score) :-
    adversaire(Jeton, Adversaire),
    (   position_gagnante(Plateau, Jeton)
    ->  Score is 10000 + Prof
    ;   position_gagnante(Plateau, Adversaire)
    ->  Score is -10000 - Prof
    ;   largeur_plateau(L),
        hauteur_plateau(H),
        nombre_coups(Plateau, NbCoups),
        MaxCoups is L * H,
        NbCoups >= MaxCoups
    ->  Score is 0
    ;   Prof =:= 0
    ->  evaluer_position(Plateau, Jeton, Score)
    ).

negamax(Plateau, Jeton, Prof, Alpha, Beta, Score) :-
    Prof > 0,
    incrementer_compteur,
    coups_possibles(Plateau, CoupsPossibles),
    (   member(Col, CoupsPossibles),
        coup_gagnant(Plateau, Col, Jeton)
    ->  Score is 10000 - Prof
    ;   negamax_recherche(Plateau, Jeton, Prof, Alpha, Beta, Score)
    ).

negamax_recherche(Plateau, Jeton, Prof, Alpha, Beta, Score) :-
    adversaire(Jeton, Adversaire),
    coups_non_perdants(Plateau, Jeton, Coups),
    (   Coups = []
    ->  Score is -9000
    ;   NouvelleProf is Prof - 1,
        trier_coups_centre(Coups, CoupsTries),
        negamax_explorer_coups(Plateau, CoupsTries, Jeton, Adversaire,
                               NouvelleProf, Alpha, Beta, Score)
    ).

trier_coups_centre(Coups, CoupsTries) :-
    sort_by_key(Coups, priorite_coup, CoupsTries).

% Vrai tri (priorité plus grande d'abord)
sort_by_key(Liste, Pred, ListeTriee) :-
    findall(Key-Item,
            (member(Item, Liste), call(Pred, Item, Key)),
            Paires),
    keysort(Paires, PairesTriees0),
    reverse(PairesTriees0, PairesTriees),
    pairs_values(PairesTriees, ListeTriee).

% Priorité pour colonnes (0-based): centre=3
priorite_coup(3, 10) :- !.
priorite_coup(2, 8) :- !.
priorite_coup(4, 8) :- !.
priorite_coup(1, 5) :- !.
priorite_coup(5, 5) :- !.
priorite_coup(_, 3).

negamax_explorer_coups(_, [], _, _, _, Alpha, _, Alpha).
negamax_explorer_coups(Plateau, [Col|Reste], Jeton, Adversaire, Prof, Alpha,
                       Beta, Score) :-
    jouer_coup(Plateau, Col, Jeton, NouveauPlateau),
    BetaNeg is -Beta,
    AlphaNeg is -Alpha,
    negamax(NouveauPlateau, Adversaire, Prof, BetaNeg, AlphaNeg, ScoreNeg),
    ScoreCoup is -ScoreNeg,
    (   ScoreCoup >= Beta
    ->  Score = ScoreCoup
    ;   ScoreCoup > Alpha
    ->  NouvelAlpha = ScoreCoup,
        negamax_explorer_coups(Plateau, Reste, Jeton, Adversaire, Prof,
                               NouvelAlpha, Beta, Score)
    ;   negamax_explorer_coups(Plateau, Reste, Jeton, Adversaire, Prof, Alpha,
                               Beta, Score)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   SOLVER PRINCIPAL   -----------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% depth IA (4 normal)
profundidad_maxima(4).

resoudre(Plateau, Jeton, Prof, Score) :-
    negamax(Plateau, Jeton, Prof, -100000, 100000, Score).

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
    ->  Score is 10000
    ;   jouer_coup(Plateau, Col, Jeton, NouveauPlateau),
        adversaire(Jeton, Adversaire),
        ProfSuivante is Prof - 1,
        resoudre(NouveauPlateau, Adversaire, ProfSuivante, ScoreAdv),
        Score is -ScoreAdv
    ).