:- module(negamax_tt, [resolve/3, solve_internal/4, clean_tt/0]).
:- use_module(negamax_base).

:- dynamic tt_entry/4. % Hash, Depth, Flag (exact, lower, upper), Value

max_depth(7). % On oeut aller plus en profondeur avec TT

clean_tt :- retractall(tt_entry(_,_,_,_)).

hash_board(Board, Token, Hash) :- term_hash(state(Board, Token), Hash).

possible_moves_ord(Board, Moves) :-
    negamax_base:width_board(W), negamax_base:height_board(H),
    Order = [3, 2, 4, 1, 5, 0, 6],
    findall(C, (member(C, Order), C < W, nth0(C, Board, Col), negamax_base:playable_col(Col, H)), Moves).

negamax_tt(Board, Token, Depth, Alpha, Beta, Score) :-
    hash_board(Board, Token, Hash),
    % 1. Lookup
    (   tt_entry(Hash, D_Store, Flag, Val), D_Store >= Depth
    ->  (Flag = exact -> Score = Val
        ; Flag = lower, Val >= Beta -> Score = Val
        ; Flag = upper, Val =< Alpha -> Score = Val
        ; fail)
    ->  true
    ;   % 2. On calcule
        negamax_calc(Board, Token, Depth, Alpha, Beta, Score, Hash)
    ).

negamax_calc(Board, Token, Depth, Alpha, Beta, Score, Hash) :-
    negamax_base:increment_nodes,
    possible_moves_ord(Board, Moves),
    (   member(C, Moves), negamax_base:winning_move(Board, C, Token) -> Score is 1000 + Depth
    ;   Depth = 0 -> Score = 0 
    ;   Moves = [] -> Score = 0
    ;   negamax_base:opponent(Token, Adv), NextDepth is Depth - 1,
        process_moves(Moves, Board, Token, Adv, NextDepth, Alpha, Beta, -99999, Score)
    ),
    % 3. Store
    (   Score =< Alpha -> Flag = upper
    ;   Score >= Beta  -> Flag = lower
    ;   Flag = exact
    ),
    retractall(tt_entry(Hash, _, _, _)),
    assertz(tt_entry(Hash, Depth, Flag, Score)).

process_moves([], _, _, _, _, _, _, Best, Best).
process_moves([Col|Rest], Board, Token, Adv, Depth, Alpha, Beta, CurrentBest, Score) :-
    negamax_base:play_move(Board, Col, Token, NewBoard),
    NB is -Beta, NA is -Alpha,
    negamax_tt(NewBoard, Adv, Depth, NB, NA, RawVal),
    Val is -RawVal,
    (   Val >= Beta -> Score = Val 
    ;   Val > CurrentBest -> 
        (Val > Alpha -> NewAlpha = Val ; NewAlpha = Alpha),
        process_moves(Rest, Board, Token, Adv, Depth, NewAlpha, Beta, Val, Score)
    ;   process_moves(Rest, Board, Token, Adv, Depth, Alpha, Beta, CurrentBest, Score)
    ).

% Point d'entrée pour le Bridge
resolve(Board, Token, Scores) :-
    clean_tt,
    max_depth(D),
    solve_internal(Board, Token, D, Scores).

% Point d'entrée pour l'ID
solve_internal(Board, Token, Depth, Scores) :-
    negamax_base:width_board(W),
    findall(Score,
            (between(0, W, C), C < W,
             (possible_moves_ord(Board, M), member(C, M)
              -> (negamax_base:winning_move(Board, C, Token) -> Score is 1000
                 ; negamax_base:play_move(Board, C, Token, NB), negamax_base:opponent(Token, Adv), D1 is Depth - 1,
                   negamax_tt(NB, Adv, D1, -10000, 10000, S), Score is -S)
              ; Score = invalid)),
            Scores).
