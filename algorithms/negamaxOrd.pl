:- module(negamax_ord, [resolve/3]).
:- use_module(negamax_base). % Reuse basic logic (win checks, etc)

% We redefine what we need to change

% Standard depth
max_depth(5). 

% OPTIMIZATION: Center First
col_order([3, 2, 4, 1, 5, 0, 6]).

possible_moves_ord(Board, Moves) :-
    negamax_base:width_board(W), negamax_base:height_board(H),
    col_order(Order),
    findall(C, (member(C, Order), C < W, nth0(C, Board, Col), negamax_base:playable_col(Col, H)), Moves).

% Optimized Negamax
negamax_ord(Board, Token, Depth, Alpha, Beta, Score) :-
    negamax_base:increment_nodes,
    possible_moves_ord(Board, Moves),
    (   member(C, Moves), negamax_base:winning_move(Board, C, Token) -> Score is 1000 + Depth
    ;   Depth = 0 -> Score = 0
    ;   Moves = [] -> Score = 0
    ;   negamax_base:opponent(Token, Adv), NextDepth is Depth - 1,
        best_score_ord(Moves, Board, Token, Adv, NextDepth, Alpha, Beta, Score)
    ).

best_score_ord([], _, _, _, _, Alpha, _, Alpha).
best_score_ord([Col|Rest], Board, Token, Adv, Depth, Alpha, Beta, Score) :-
    negamax_base:play_move(Board, Col, Token, NewBoard),
    NB is -Beta, NA is -Alpha,
    negamax_ord(NewBoard, Adv, Depth, NB, NA, RawScore),
    Val is -RawScore,
    (   Val >= Beta -> Score = Val
    ;   Val > Alpha -> best_score_ord(Rest, Board, Token, Adv, Depth, Val, Beta, Score)
    ;   best_score_ord(Rest, Board, Token, Adv, Depth, Alpha, Beta, Score)
    ).

resolve(Board, Token, Scores) :-
    max_depth(D), negamax_base:width_board(W),
    findall(Score,
            (between(0, W, C), C < W,
             (possible_moves_ord(Board, M), member(C, M)
              -> (negamax_base:winning_move(Board, C, Token) -> Score is 1000
                 ; negamax_base:play_move(Board, C, Token, NB), negamax_base:opponent(Token, Adv), D1 is D-1,
                   negamax_ord(NB, Adv, D1, -10000, 10000, S), Score is -S)
              ; Score = invalid)),
            Scores).