:- consult('negamax.pl').

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
    format(user_error, '~n=== IA NEGAMAX ACTIVÉE ===~n', []),
    format(user_error, 'Couleur joueur: ~w~n', [CouleurJoueur]),
    convertir_jeton(CouleurJoueur, JetonNegamax),
    format(user_error, 'Jeton Negamax: ~w~n', [JetonNegamax]),
    convertir_plateau_vers_negamax(PlateauNegamax),
    format(user_error, 'Plateau converti: ~w~n', [PlateauNegamax]),
    format(user_error, 'Appel de analyser/3...~n', []),
    analyser(PlateauNegamax, JetonNegamax, ScoresBruts),
    format(user_error, 'Scores bruts: ~w~n', [ScoresBruts]),
    filtrer_scores_jouables(ScoresBruts, ScoresFiltres),
    format(user_error, 'Scores filtrés: ~w~n', [ScoresFiltres]),
    (   trouver_meilleur_index(ScoresFiltres, 1, Mouvement)
    ->  format(user_error, 'Meilleur coup: colonne ~w~n', [Mouvement]),
        (   colonne_jouable_main(Mouvement)
        ->  format(user_error, 'Colonne ~w confirmée jouable~n', [Mouvement])
        ;   format(user_error, 'ERREUR: Colonne ~w non jouable~n', [Mouvement]),
            fail
        ),
        (   compteur_noeuds(N)
        ->  format(user_error, 'Noeuds explorés: ~w~n', [N])
        ;   true
        )
    ;   format(user_error, 'Aucun coup valide trouvé~n', []),
        fail
    ).

ia_choisir_coup(_, Mouvement) :-
    format(user_error, 'Fallback activé~n', []),
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
    format(user_error, ' ~n=== TEST DE CONVERSION ===~n', []),
    convertir_plateau_vers_negamax(Plateau),
    format(user_error, ' Plateau: ~w~n', [Plateau]),
    format(user_error, ' ~nColonnes jouables:~n', []),
    forall(between(1, 7, Col),
           (colonne_jouable_main(Col)
           -> format(user_error, '   Colonne ~w: jouable~n', [Col])
           ;  format(user_error, '   Colonne ~w: pleine~n', [Col]))).