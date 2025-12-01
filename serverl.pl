:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(apply)).
:- use_module(library(lists)). 

% Charge la logique du jeu Puissance 4.
:- consult('puissance4.pl').

% Démarre le serveur HTTP sur le port 8080.
start_server :-
    http_server(http_dispatch, [port(8080)]).

% Gère les fichiers web statiques (HTML, CSS, JS) depuis le dossier 'view/'.
:- http_handler(root(.), serve_web_files, [prefix]).
serve_web_files(Requete) :-
    member(path(Chemin), Requete),
    (   Chemin = '/'
    ->  http_reply_file('view/index.html', [], [pass_to_os(true)])
    ;   http_reply_from_files('view', [], Requete)
    ).

% ======================================================================
% ENDPOINTS
% ======================================================================

% Endpoint pour obtenir l'état initial du plateau.
:- http_handler(root(api/init_game), init_game_handler, [methods([post])]).
init_game_handler(_Requete) :-
    init_board, % Initialise les faits column/3
    faits_prolog_vers_json_plateau(JSONPlateau), % Convertit les faits en JSON
    reply_json(json{board: JSONPlateau, player: 'r', status: 'playing'}).


% Endpoint pour effectuer un mouvement.
:- http_handler(root(api/move), move_handler, [methods([post])]).
move_handler(Requete) :-
    http_read_json_dict(Requete, JSONEntree),
    (   get_dict(column, JSONEntree, NumeroCol),
        get_dict(player, JSONEntree, AtomeJoueur)
    ->  Joueur = AtomeJoueur,
        (   est_mouvement_valide(NumeroCol) % Vérifie la validité avec les faits
        ->  placer_jeton_modifie_faits(NumeroCol, Joueur), % Modifie les faits column/3
            faits_prolog_vers_matrice_plateau(NouvelleMatricePlateau), % Reconstruit la matrice 2D pour les checks
            (   hay_ganador(NouvelleMatricePlateau, Gagnant)
            ->  Statut = winner(Gagnant)
            ;   tablero_lleno(NouvelleMatricePlateau)
            ->  Statut = draw
            ;   (Joueur = r -> JoueurSuivant = a ; JoueurSuivant = r),
                Statut = playing
            ),
            faits_prolog_vers_json_plateau(JSONPlateau) % Relit les faits pour la réponse
        ;   % Mouvement invalide
            Statut = invalid_move,
            JoueurSuivant = Joueur, % Le joueur actuel rejoue
            faits_prolog_vers_json_plateau(JSONPlateau) % Le plateau ne change pas
        ),
        reply_json(json{board: JSONPlateau, player: JoueurSuivant, status: Statut})
    ;   http_global:reply_json_dict(json{error: 'Requête JSON invalide'}),
        throw(http_reply(bad_request('Requête JSON invalide')))
    ).

% Helper: Vérifie si un mouvement est valide pour une colonne.
est_mouvement_valide(NumeroCol) :-
    num_cols(NbCols), num_rows(NbLignes),
    NumeroCol >= 1, NumeroCol =< NbCols,
    column(NumeroCol, _, DernierePos),
    DernierePos =< NbLignes.

% Helper: Place le jeton en modifiant les faits (appelle la logique de puissance4.pl)
placer_jeton_modifie_faits(NumeroCol, Joueur) :-
    colocar_ficha(nil, NumeroCol, Joueur, nil).


% Endpoint pour demander un mouvement à l'IA.
:- http_handler(root(api/ai_move), ai_move_handler, [methods([post])]).
ai_move_handler(Requete) :-
    http_read_json_dict(Requete, JSONEntree),
    (   get_dict(player, JSONEntree, JoueurIA),
        get_dict(opponent, JSONEntree, JoueurAdversaire)
    ->  % Reconstruire la matrice 2D à partir des faits pour l'IA
        faits_prolog_vers_matrice_plateau(MatricePlateauActuelleIA),

        ProfondeurIA = 2, % Profondeur de recherche de l'IA
        (   elegir_movimiento_ia(MatricePlateauActuelleIA, ProfondeurIA, JoueurIA, JoueurAdversaire, ColonneChoisie)
        ->  % L'IA a choisi une colonne, applique le mouvement en modifiant les faits
            placer_jeton_modifie_faits(ColonneChoisie, JoueurIA),
            faits_prolog_vers_matrice_plateau(NouvelleMatricePlateauIA), % Reconstruit pour les checks

            (   hay_ganador(NouvelleMatricePlateauIA, Gagnant)
            ->  Statut = winner(Gagnant)
            ;   tablero_lleno(NouvelleMatricePlateauIA)
            ->  Statut = draw
            ;   Statut = playing
            ),
            faits_prolog_vers_json_plateau(JSONPlateau), % Relit les faits pour la réponse
            reply_json(json{columnChosen: ColonneChoisie, board: JSONPlateau, player: JoueurAdversaire, status: Statut})
        ;   reply_json(json{error: 'L\'IA n\'a pas pu faire de mouvement. La logique IA est-elle implémentée?'}),
            throw(http_reply(bad_request('Erreur IA')))
        )
    ;   http_global:reply_json_dict(json{error: 'Requête IA JSON invalide'}),
        throw(http_reply(bad_request('Requête IA JSON invalide')))
    ).


% --- HELPERS: Conversion FAITS Prolog (column/3) <-> JSON (matrice 2D) ---

% faits_prolog_vers_json_plateau(-JSONPlateau)
% Lit les faits column/3, construit une matrice 2D et la convertit en JSON.
faits_prolog_vers_json_plateau(JSONPlateau) :-
    faits_prolog_vers_matrice_plateau(MatricePlateau),
    num_rows(NbLignes),
    maplist(ligne_vers_liste_json, MatricePlateau, JSONPlateau).

ligne_vers_liste_json(LigneProlog, ListeJSON) :-
    maplist(cellule_vers_atome_json, LigneProlog, ListeJSON).

cellule_vers_atome_json(v, ' ').
cellule_vers_atome_json(r, 'r').
cellule_vers_atome_json(a, 'a').


% faits_prolog_vers_matrice_plateau(-MatricePlateau)
% Lit les faits column/3 et construit une matrice 2D (liste de listes).
% Les lignes sont du haut (index 0) vers le bas.
faits_prolog_vers_matrice_plateau(MatricePlateau) :-
    num_rows(NbLignes), num_cols(NbCols),
    findall(ListeLigne, (
        between(0, NbLignes - 1, IndexLigneMatrice),
            IndexLigneProlog is NbLignes - IndexLigneMatrice, % Convertit l'index de matrice (0-basé) en index Prolog (1-basé, du bas)        
        findall(ValeurCellule, (
            between(1, NbCols, NumeroCol),
            column(NumeroCol, DonneesCol, _), % Obtient les données de la colonne
            nth1(IndexLigneProlog, DonneesCol, ValeurCellule) % Obtient la cellule spécifique
        ), ListeLigne)
    ), MatricePlateau).

% La conversion JSON -> PrologMatrix n'est pas directement utilisée pour modifier les faits column/3,
% mais est utile si d'autres parties du code Prolog ont besoin de travailler avec une matrice 2D.
json_vers_matrice_prolog(JSONPlateau, MatricePlateauProlog) :-
    maplist(liste_json_vers_ligne, JSONPlateau, MatricePlateauProlog).

liste_json_vers_ligne(ListeJSON, LigneProlog) :-
    maplist(atome_json_vers_cellule, ListeJSON, LigneProlog).

atome_json_vers_cellule(' ', v).
atome_json_vers_cellule('r', r).
atome_json_vers_cellule('a', a).