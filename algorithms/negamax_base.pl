:- module(negamax_base, [resolve/3]).

% Configuration
width_board(7).
height_board(6).
max_depth(4). 

opponent(x, o).
opponent(o, x).

playable_col(Col, Height) :- length(Col, L), L < Height.

% Possible moves (Standard Order: 0..6)
possible_moves(Board, Moves) :-
    width_board(W), height_board(H),
    findall(C, (between(0, W, C), C < W, nth0(C, Board, Col), playable_col(Col, H)), Moves).

play_move(Board, C, Token, NewBoard) :-
    nth0(C, Board, Col, Rest),
    append(Col, [Token], NewCol),
    nth0(C, NewBoard, NewCol, Rest).

% Victory Detection (Simplified for brevity, assumes standard connect4 logic exists)
winning_move(Board, Col, Token) :- play_move(Board, Col, Token, NB), check_win(NB, Token).
check_win(Board, Token) :- (win_h(Board, Token); win_v(Board, Token); win_d(Board, Token)).

% --- Victory Checks ---
win_v(B, T) :- member(C, B), sublist(T, C, 4).
win_h(B, T) :- height_board(H), between(0, H, R), get_row(B, R, Row), sublist(T, Row, 4).
win_d(B, T) :- width_board(W), height_board(H), between(0, W, C), between(0, H, R), check_diag(B, C, R, 1, 1, T).
win_d(B, T) :- width_board(W), height_board(H), between(0, W, C), between(0, H, R), check_diag(B, C, R, 1, -1, T).

check_diag(B, C, R, DC, DR, T) :- check_diag_aux(B, C, R, DC, DR, T, 0).
check_diag_aux(_, _, _, _, _, _, 4) :- !.
check_diag_aux(B, C, R, DC, DR, T, Acc) :-
    nth0(C, B, Col), nth0(R, Col, T), !,
    Acc1 is Acc + 1, C1 is C + DC, R1 is R + DR,
    check_diag_aux(B, C1, R1, DC, DR, T, Acc1).

sublist(E, L, N) :- append(_, Rest, L), prefix_len(Rest, E, Len), Len >= N.
prefix_len([E|T], E, L) :- !, prefix_len(T, E, L1), L is L1 + 1.
prefix_len(_, _, 0).

get_row(Board, RowNum, Row) :- findall(X, (member(C, Board), length(C, L), L > RowNum, nth0(RowNum, C, X)), Row).

% --- Negamax ---
increment_nodes :- 
    (retract(user:compteur_noeuds(N)) -> N1 is N+1, assertz(user:compteur_noeuds(N1)); true).

negamax(Board, Token, Depth, Alpha, Beta, Score) :-
    increment_nodes,
    possible_moves(Board, Moves),
    (   member(C, Moves), winning_move(Board, C, Token) -> Score is 1000 + Depth
    ;   Depth = 0 -> Score = 0
    ;   Moves = [] -> Score = 0
    ;   opponent(Token, Adv), NextDepth is Depth - 1,
        best_score(Moves, Board, Token, Adv, NextDepth, Alpha, Beta, Score)
    ).

best_score([], _, _, _, _, Alpha, _, Alpha).
best_score([Col|Rest], Board, Token, Adv, Depth, Alpha, Beta, Score) :-
    play_move(Board, Col, Token, NewBoard),
    NB is -Beta, NA is -Alpha,
    negamax(NewBoard, Adv, Depth, NB, NA, RawScore),
    Val is -RawScore,
    (   Val >= Beta -> Score = Val
    ;   Val > Alpha -> best_score(Rest, Board, Token, Adv, Depth, Val, Beta, Score)
    ;   best_score(Rest, Board, Token, Adv, Depth, Alpha, Beta, Score)
    ).

% Entry Point
resolve(Board, Token, Scores) :-
    max_depth(D), width_board(W),
    findall(Score,
            (between(0, W, C), C < W,
             (possible_moves(Board, M), member(C, M)
              -> (winning_move(Board, C, Token) -> Score is 1000
                 ; play_move(Board, C, Token, NB), opponent(Token, Adv), D1 is D-1,
                   negamax(NB, Adv, D1, -10000, 10000, S), Score is -S)
              ; Score = invalid)),
            Scores).