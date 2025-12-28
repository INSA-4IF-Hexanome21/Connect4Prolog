:- module(ai, [ia_choose_move/3]).
:- use_module(library(lists)).

% Load all AI implementations aliased to avoid name conflicts
:- use_module('negamax_base', [resolve/3 as solve_base]).
:- use_module('negamax_ord',  [resolve/3 as solve_ord]).
:- use_module('negamax_tt',   [resolve/3 as solve_tt]).
:- use_module('negamax_id',   [resolve/3 as solve_id]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% -----   ADAPTER / BRIDGE   --------------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Retrieve column data from the Main 'user' module
get_column_from_main(NumCol, Data) :-
    user:column(NumCol, Data, _).

% Convert Main Board format (e, RED, YELLOW) -> AI format (x, o)
convert_board(BoardAI) :-
    findall(CleanCol,
            (between(1, 7, ColNum),
             get_column_from_main(ColNum, RawCol),
             clean_column(RawCol, CleanCol)),
            BoardAI).

clean_column(RawList, CleanList) :-
    findall(Val,
            (member(Elem, RawList),
             Elem \= 'e', % Ignore empty cells
             convert_token(Elem, Val)),
            CleanList).

% Token mapping
convert_token('YELLOW', x). 
convert_token('RED', o).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   LOGIC ROUTER   ---------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_choose_move(PlayerColor, Move, Strategy) :-
    convert_token(PlayerColor, TokenAI),
    convert_board(Board),
    
    % Statistics initialization
    (current_predicate(user:compteur_noeuds/1) -> retractall(user:compteur_noeuds(_)); true),
    assertz(user:compteur_noeuds(0)),

    % Route to the specific implementation
    (   Strategy == base -> solve_base(Board, TokenAI, Scores)
    ;   Strategy == ord  -> solve_ord(Board, TokenAI, Scores)
    ;   Strategy == tt   -> solve_tt(Board, TokenAI, Scores)
    ;   Strategy == id   -> solve_id(Board, TokenAI, Scores)
    ;   writeln('Unknown strategy, defaulting to BASE'), solve_base(Board, TokenAI, Scores)
    ),

    % Select the best move from the scores
    write('Scores: '), writeln(Scores),
    find_best_index(Scores, Move),
    
    % Print stats
    user:compteur_noeuds(N),
    format(user_error, ' Nodes explored: ~w~n', [N]).

% Fallback if something fails
ia_choose_move(_, Move, _) :-
    writeln('AI Error: Playing Random.'),
    random_between(1, 7, Move).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   UTILITIES   ------------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_best_index(Scores, Index) :-
    find_best_index_aux(Scores, 1, -999999, 0, Index),
    Index > 0.

find_best_index_aux([], _, _, BestIdx, BestIdx).
find_best_index_aux([Score|Rest], Pos, MaxScore, BestIdx, Result) :-
    NextPos is Pos + 1,
    (   Score \= invalid, Score > MaxScore
    ->  find_best_index_aux(Rest, NextPos, Score, Pos, Result)
    ;   find_best_index_aux(Rest, NextPos, MaxScore, BestIdx, Result)
    ).