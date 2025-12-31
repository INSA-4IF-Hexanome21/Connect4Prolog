:- module(negamax_id, [resolve/3]).
:- use_module(negamax_tt).

% On veut une grande profondueur parceque l'ID le permet
final_depth(8). 

resolve(Board, Token, Scores) :-
    % 1. On nettoie la mémoire
    negamax_tt:clean_tt,
    final_depth(Max),
    Limit is Max - 1,
    
    % 2. On boucle la profonduer entre 1 et Max - 1 pour le cache 
    forall(between(1, Limit, D),
           (
            % Nous ignorons les scores des recherches peu profondes. Nous ne conservons que les effets de bord de la table de transposition (TT)
            negamax_tt:solve_internal(Board, Token, D, _)
           )),
           
    % 3. Recherche finale
    negamax_tt:solve_internal(Board, Token, Max, Scores).
