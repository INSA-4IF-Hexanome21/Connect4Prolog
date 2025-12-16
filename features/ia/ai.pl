:- consult('features/ia/negamax.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% -----   ADAPTATEUR (BRIDGE)   ------------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obtenir_colonne(NumCol, DonneesCol) :-
    column(NumCol, DonneesCol, _).
% 2. Convertir TOUT le plateau format Main vers format Negamax
% Main: Colonnes 1..7, contiennent 'e', 'RED', 'YELLOW'
% Negamax: Liste de 7 listes, contiennent 'x', 'o' (pas de vides)
convertir_plateau_vers_negamax(PlateauNegamax) :-
    findall(ColNettoyee,
            (between(1, 7, NumCol), obtenir_colonne(NumCol, ColBrute),
             nettoyer_colonne(ColBrute, ColNettoyee)),
            PlateauNegamax).
% Enlève les 'e' et convertit les couleurs
nettoyer_colonne(ListeBrute, ListePropre) :-
    findall(JetonConverti,
            (member(Jeton, ListeBrute), Jeton \= 'e',
             convertir_jeton(Jeton, JetonConverti)),
            ListePropre).

% Dictionnaire de traduction
convertir_jeton('Y', x).
convertir_jeton('R', o).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   LOGIQUE (ALGO overall)   -------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

colonne_jouable_main(NumCol) :-
    column(NumCol, _, LastPos),
    LastPos < 6.

filtrer_scores_jouables(Scores, ScoresJouables) :-
    filtrer_scores_jouables_aux(Scores, 1, ScoresJouables).

filtrer_scores_jouables_aux([], _, []).
filtrer_scores_jouables_aux([Score|Reste], NumCol, [ScoreFiltre|ResteFiltre]) :-
    NextCol is NumCol + 1,
    (   colonne_jouable_main(NumCol),
        Score \= invalide
    ->  ScoreFiltre = Score
    ;   ScoreFiltre = invalide
    ),
    filtrer_scores_jouables_aux(Reste, NextCol, ResteFiltre).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% -----   PREDICAT PRINCIPAL IA   -----------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_choisir_coup(CouleurJoueur, Mouvement) :-
    % format('~n=== IA NEGAMAX ACTIVÉE ===~n', []),
    % format('Couleur joueur: ~w~n', [CouleurJoueur]),

    convertir_jeton(CouleurJoueur, JetonNegamax),
    % format('Jeton Negamax: ~w~n', [JetonNegamax]),

    convertir_plateau_vers_negamax(PlateauNegamax),
    % format('Plateau converti: ~w~n', [PlateauNegamax]),

    % format('Appel de analyser/3...~n', []),
    analyser(PlateauNegamax, JetonNegamax, ScoresBruts),
    % format('Scores bruts: ~w~n', [ScoresBruts]),

    filtrer_scores_jouables(ScoresBruts, ScoresFiltres),
    % format('Scores filtrés: ~w~n', [ScoresFiltres]),

    (   trouver_meilleur_index(ScoresFiltres, 1, Mouvement)
    ->  % format('Meilleur coup: colonne ~w~n', [Mouvement]),
        (   colonne_jouable_main(Mouvement)
        ->  % format('Colonne ~w confirmée jouable~n', [Mouvement])
            true
        ;   % format('ERREUR: Colonne ~w non jouable~n', [Mouvement]),
            fail
        ),
        (   compteur_noeuds(_N)
        ->  % format('Noeuds explorés: ~w~n', [_N])
            true
        ;   true
        )
    ;   % format('Aucun coup valide trouvé~n', []),
        fail
    ).

ia_choisir_coup(_, Mouvement) :-
    %format('Fallback activé~n', []),
    repeat,
    random(1, 8, Mouvement),
    colonne_jouable_main(Mouvement),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   UTILITAIRES   ----------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trouver_meilleur_index(Scores, IndexDebut, MeilleurIndex) :-
    trouver_meilleur_index_aux(Scores, IndexDebut, -100000, 0, MeilleurIndex),
    MeilleurIndex > 0.

trouver_meilleur_index_aux([], _, _, MeilleurIdx, MeilleurIdx) :-
    MeilleurIdx > 0.
trouver_meilleur_index_aux([Score|Reste], IndexCourant, MeilleurScoreActuel,
                           MeilleurIndexActuel, Resultat) :-
    IndexSuivant is IndexCourant + 1,
    (   Score \= invalide,
        number(Score),
        Score > MeilleurScoreActuel
    ->  trouver_meilleur_index_aux(Reste, IndexSuivant, Score, IndexCourant,
                                  Resultat)
    ;   trouver_meilleur_index_aux(Reste, IndexSuivant, MeilleurScoreActuel,
                                  MeilleurIndexActuel, Resultat)
    ).

test_conversion :-
    format('~n=== TEST DE CONVERSION ===~n', []),
    convertir_plateau_vers_negamax(Plateau),
    format('Plateau: ~w~n', [Plateau]),
    format('~nColonnes jouables:~n', []),
    forall(between(1, 7, Col),
           (colonne_jouable_main(Col)
           -> format('  Colonne ~w: jouable~n', [Col])
           ;  format('  Colonne ~w: pleine~n', [Col]))).