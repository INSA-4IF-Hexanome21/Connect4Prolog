:- module(ai, [ia_choisir_coup/2]).
:- consult('negamax.pl').
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% -----   ADAPTATEUR (BRIDGE)   ------------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Lire les colonnes depuis le module 'user' (le fichier principal)
get_column_from_main(NumCol, DonneesCol) :-
    user:column(NumCol, DonneesCol, _).

% 2. Convertir TOUT le plateau format Main vers format Negamax
% Main: Colonnes 1..7, contiennent 'e', 'RED', 'YELLOW'
% Negamax: Liste de 7 listes, contiennent 'x', 'o' (pas de vides)
convertir_plateau(PlateauNegamax) :-
    findall(ColNettoyee,
            (between(1, 7, NumCol),
             get_column_from_main(NumCol, Brut),
             nettoyer_colonne(Brut, ColNettoyee)),
            PlateauNegamax).

% Enlève les 'e' et convertit les couleurs
nettoyer_colonne(ListeBrute, ListePropre) :-
    findall(Val,
            (member(Elem, ListeBrute),
             Elem \= 'e',              % On vire les cases vides pour Negamax
             convertir_jeton(Elem, Val)),
            ListePropre).

% Dictionnaire de traduction
convertir_jeton('YELLOW', x). % Si YELLOW est x dans negamax
convertir_jeton('RED', o).    % Si RED est o dans negamax

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   LOGIQUE (ALGO overall)   -------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_choisir_coup(Joueur, Mouvement) :-
    % 1. Conversion du nom du joueur (RED -> o)
    convertir_jeton(Joueur, JetonNegamax),
    
    % 2. Conversion du plateau (user:column -> Liste de listes)
    convertir_plateau(Plateau),
    
    % 3. Appel au fichier negamax.pl
    % Note: analyser/3 renvoie une liste de 7 scores correspondant aux colonnes 0..6
    write('IA (Negamax) analyse le plateau...'), nl,
    analyser(Plateau, JetonNegamax, Scores),
    
    write('Scores par colonne (de 1 a 7) : '), writeln(Scores),

    % 4. Trouver le meilleur index (1-based pour le Main)
    trouver_meilleur_index(Scores, Mouvement),
    
    % Debug stats
    (current_predicate(compteur_noeuds/1), compteur_noeuds(N) -> 
        write('Noeuds explores : '), writeln(N)
    ; true).

% Si Negamax renvoie tout "invalide" (colonne pleine ou bug), on joue aléatoire
ia_choisir_coup(_, Mouvement) :-
    writeln('Negamax n\'a rien trouve, coup aleatoire.'),
    ia_aleatoire(Mouvement).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   UTILITAIRES   ----------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Trouve l'index (1..7) de la valeur maximale dans la liste des scores
trouver_meilleur_index(Scores, Index) :-
    trouver_meilleur_index_aux(Scores, 1, -100000, 0, Index),
    Index > 0. % Vérifie qu'on a trouvé quelque chose valide

% Base case
trouver_meilleur_index_aux([], _, _, MeilleurIndex, MeilleurIndex).

% Recursive
trouver_meilleur_index_aux([Score|Reste], Cursor, MaxActuel, IndexActuel, Resultat) :-
    NextCursor is Cursor + 1,
    (   Score \= invalide, Score > MaxActuel
    ->  % Nouveau meilleur score trouvé
        trouver_meilleur_index_aux(Reste, NextCursor, Score, Cursor, Resultat)
    ;   % Sinon on continue
        trouver_meilleur_index_aux(Reste, NextCursor, MaxActuel, IndexActuel, Resultat)
    ).

