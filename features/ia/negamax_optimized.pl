:- module(negamax, [
    analyser/3,
    compteur_noeuds/1
]).

:- dynamic compteur_noeuds/1.
:- dynamic lines_cache/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CONFIGURATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

largeur(7).
hauteur(6).
profondeur_recherche(12).  % Profundidad razonable

% Bitboard masks
bottom_mask(4432676798593).
board_mask(279258638311359).

% ORDEN ALEATORIZADO o centro primero con más peso
move_order([3, 2, 4, 1, 5, 0, 6]).

mate_value(1000000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CORE BITBOARD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bottom_col(Col, Bit) :-
    Bit is 1 << (Col * 7).

column_mask(Col, Mask) :-
    Mask is 127 << (Col * 7).

top_mask_playable(Col, Bit) :-
    Bit is 1 << (Col * 7 + 5).

can_play(Mask, Col) :-
    between(0, 6, Col),
    top_mask_playable(Col, Top),
    (Mask /\ Top) =:= 0.

move_bit(Mask, Col, MoveBit) :-
    bottom_col(Col, Bottom),
    column_mask(Col, ColMask),
    MoveBit is (Mask + Bottom) /\ ColMask.

make_move(Pos, Mask, Col, NewPos, NewMask, MoveBit) :-
    move_bit(Mask, Col, MoveBit),
    NewMask is Mask \/ MoveBit,
    NewPos is Pos xor Mask.

playable_mask(Mask, Playable) :-
    bottom_mask(Bottom),
    board_mask(Board),
    Playable is (Mask + Bottom) /\ Board.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WINNING DETECTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_winning(Pos) :-
    M1 is Pos /\ (Pos >> 7),
    (M1 /\ (M1 >> 14)) =\= 0, !.
is_winning(Pos) :-
    M2 is Pos /\ (Pos >> 1),
    (M2 /\ (M2 >> 2)) =\= 0, !.
is_winning(Pos) :-
    M3 is Pos /\ (Pos >> 6),
    (M3 /\ (M3 >> 12)) =\= 0, !.
is_winning(Pos) :-
    M4 is Pos /\ (Pos >> 8),
    (M4 /\ (M4 >> 16)) =\= 0, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% POPCOUNT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

popcount(0, 0) :- !.
popcount(N, C) :-
    N1 is N /\ (N - 1),
    popcount(N1, C1),
    C is C1 + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LINES (69 masks)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lines(Lines) :-
    lines_cache(Lines), !.
lines(Lines) :-
    findall(M, line_mask(M), Lines),
    retractall(lines_cache(_)),
    assertz(lines_cache(Lines)).

line_mask(Mask) :-
    between(0, 3, C0), between(0, 5, R0),
    line_from(C0, R0, 1, 0, Mask).
line_mask(Mask) :-
    between(0, 6, C0), between(0, 2, R0),
    line_from(C0, R0, 0, 1, Mask).
line_mask(Mask) :-
    between(0, 3, C0), between(0, 2, R0),
    line_from(C0, R0, 1, 1, Mask).
line_mask(Mask) :-
    between(0, 3, C0), between(3, 5, R0),
    line_from(C0, R0, 1, -1, Mask).

line_from(C0, R0, DC, DR, Mask) :-
    bit_at(C0, R0, B0),
    C1 is C0 + DC, R1 is R0 + DR, bit_at(C1, R1, B1),
    C2 is C0 + 2*DC, R2 is R0 + 2*DR, bit_at(C2, R2, B2),
    C3 is C0 + 3*DC, R3 is R0 + 3*DR, bit_at(C3, R3, B3),
    Mask is B0 \/ B1 \/ B2 \/ B3.

bit_at(Col, Row, Bit) :-
    Bit is 1 << (Col * 7 + Row).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HEURÍSTICA MEJORADA - EVALUACIÓN COMPLETA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. AMENAZAS INMEDIATAS (Win-in-1)
count_win_in1(Pos, Mask, Count, WinMoveMask) :-
    move_order(Cols),
    count_win_in1_(Cols, Pos, Mask, 0, 0, Count, WinMoveMask).

count_win_in1_([], _, _, AccC, AccM, AccC, AccM).
count_win_in1_([Col|Rest], Pos, Mask, AccC, AccM, Count, WinMoveMask) :-
    (   can_play(Mask, Col)
    ->  make_move(Pos, Mask, Col, NewPos, NewMask, MoveBit),
        JustPlayedPos is NewMask xor NewPos,
        (   is_winning(JustPlayedPos)
        ->  AccC1 is AccC + 1, AccM1 is AccM \/ MoveBit
        ;   AccC1 = AccC, AccM1 = AccM
        ),
        count_win_in1_(Rest, Pos, Mask, AccC1, AccM1, Count, WinMoveMask)
    ;   count_win_in1_(Rest, Pos, Mask, AccC, AccM, Count, WinMoveMask)
    ).

% 2. CONECTIVIDAD FUERTE (2-en-línea, 3-en-línea)
connectivity_score(Pos, OppPos, Score) :-
    lines(Lines),
    board_mask(Board),
    Empty is Board xor (Pos \/ OppPos),
    connectivity_score_(Lines, Pos, OppPos, Empty, 0, Score).

connectivity_score_([], _, _, _, Acc, Acc).
connectivity_score_([L|Rest], Pos, OppPos, Empty, Acc, Score) :-
    MyIn is L /\ Pos,
    OpIn is L /\ OppPos,
    EmIn is L /\ Empty,
    popcount(MyIn, MC),
    popcount(OpIn, OC),
    popcount(EmIn, EC),
    
    % Solo líneas abiertas (sin oponente)
    (   OC =:= 0
    ->  (   MC =:= 3, EC =:= 1 -> MyVal = 500      % 3-en-línea abierta
        ;   MC =:= 2, EC =:= 2 -> MyVal = 50       % 2-en-línea abierta
        ;   MC =:= 1, EC =:= 3 -> MyVal = 5        % 1-en-línea abierta
        ;   MyVal = 0
        )
    ;   MyVal = 0
    ),
    
    % Idem para oponente
    (   MC =:= 0
    ->  (   OC =:= 3, EC =:= 1 -> OpVal = 600      % Amenaza enemiga más importante
        ;   OC =:= 2, EC =:= 2 -> OpVal = 60
        ;   OC =:= 1, EC =:= 3 -> OpVal = 6
        ;   OpVal = 0
        )
    ;   OpVal = 0
    ),
    
    Acc1 is Acc + MyVal - OpVal,
    connectivity_score_(Rest, Pos, OppPos, Empty, Acc1, Score).

% 3. CONTROL DE COLUMNAS (altura relativa)
column_control_score(Pos, OppPos, Mask, Score) :-
    findall(ColScore, (
        between(0, 6, Col),
        column_control_col(Pos, OppPos, Mask, Col, ColScore)
    ), Scores),
    sumlist(Scores, Score).

column_control_col(Pos, OppPos, Mask, Col, Score) :-
    column_mask(Col, CMask),
    MyCol is Pos /\ CMask,
    OpCol is OppPos /\ CMask,
    ColMask is Mask /\ CMask,
    popcount(MyCol, MC),
    popcount(OpCol, OC),
    popcount(ColMask, Total),
    
    % Bonus por tener más fichas en la columna
    Diff is MC - OC,
    % Bonus por controlar la parte superior
    (   Total < 6
    ->  bottom_col(Col, Bottom),
        TopBit is (ColMask + Bottom) /\ CMask,
        (   (TopBit /\ Pos) =\= 0 -> TopBonus = 3
        ;   (TopBit /\ OppPos) =\= 0 -> TopBonus = -3
        ;   TopBonus = 0
        )
    ;   TopBonus = 0
    ),
    
    Score is Diff * 2 + TopBonus.

% 4. CENTRO (columnas 2, 3, 4)
center_mask(17044586496).

center_score(Pos, OppPos, Score) :-
    center_mask(CM),
    popcount(Pos /\ CM, MC),
    popcount(OppPos /\ CM, OC),
    Score is (MC - OC) * 10.  % Mayor peso al centro

% 5. ALTURA FAVORABLE (evitar dejar plataformas al oponente)
height_safety_score(Pos, OppPos, Mask, Score) :-
    findall(H, (
        between(0, 6, Col),
        height_safety_col(Pos, OppPos, Mask, Col, H)
    ), Heights),
    sumlist(Heights, Score).

height_safety_col(Pos, OppPos, Mask, Col, Score) :-
    column_mask(Col, CMask),
    ColMask is Mask /\ CMask,
    popcount(ColMask, Height),
    
    % Penalizar alturas pares si el oponente está arriba
    (   Height mod 2 =:= 0,
        Height > 0,
        Height < 6
    ->  bottom_col(Col, Bottom),
        NextBit is (ColMask + Bottom) /\ CMask,
        PrevBit is ColMask xor NextBit,
        % Si la última ficha es del oponente, malo
        (   (PrevBit /\ OppPos) =\= 0 -> Score = -8
        ;   Score = 0
        )
    ;   Score = 0
    ).

% EVALUACIÓN COMBINADA
eval_position(Pos, Mask, Score) :-
    OppPos is Mask xor Pos,
    
    % 1. Amenazas inmediatas (muy importante)
    count_win_in1(Pos, Mask, MyWin1, _),
    count_win_in1(OppPos, Mask, OpWin1, _),
    
    % 2. Conectividad (líneas abiertas)
    connectivity_score(Pos, OppPos, ConnScore),
    
    % 3. Centro
    center_score(Pos, OppPos, CenterScore),
    
    % 4. Control de columnas
    column_control_score(Pos, OppPos, Mask, ColScore),
    
    % 5. Seguridad de altura
    height_safety_score(Pos, OppPos, Mask, HeightScore),
    
    % Pesos ajustados
    WinBonus is (MyWin1 >= 2 -> 8000 ; 0),  % Doble amenaza
    
    Score is MyWin1 * 15000          % Mis amenazas inmediatas
          - OpWin1 * 18000           % Sus amenazas (más peso)
          + ConnScore                % Conectividad
          + CenterScore              % Centro
          + ColScore                 % Control columnas
          + HeightScore              % Seguridad altura
          + WinBonus.                % Bonus fork

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NEGAMAX CON FILTRO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inc_node :-
    (   retract(compteur_noeuds(N))
    ->  N1 is N + 1, assertz(compteur_noeuds(N1))
    ;   assertz(compteur_noeuds(1))
    ).

negamax(Pos, Mask, Depth, Alpha, Beta, Score) :-
    inc_node,
    OppPos is Mask xor Pos,
    mate_value(Mate),
    (   is_winning(OppPos)
    ->  Score is -Mate - Depth
    ;   board_mask(Board), Mask =:= Board
    ->  Score is 0
    ;   Depth =:= 0
    ->  eval_position(Pos, Mask, Score)
    ;   count_win_in1(Pos, Mask, MyWin1, _),
        MyWin1 > 0
    ->  Score is Mate + Depth
    ;   search_moves(Pos, Mask, Depth, Alpha, Beta, -Mate, Score)
    ).

search_moves(Pos, Mask, Depth, Alpha, Beta, Best, FinalScore) :-
    move_order(Cols),
    search_cols(Cols, Pos, Mask, Depth, Alpha, Beta, Best, FinalScore).

search_cols([], _, _, _, _, _, Best, Best).
search_cols([Col|Rest], Pos, Mask, Depth, Alpha, Beta, Best, Final) :-
    mate_value(Mate),
    (   can_play(Mask, Col)
    ->  make_move(Pos, Mask, Col, NewPos, NewMask, _),
        JustPlayedPos is NewMask xor NewPos,
        (   is_winning(JustPlayedPos)
        ->  Score is Mate + Depth
        ;   count_win_in1(NewPos, NewMask, OppWin1, _),
            (   OppWin1 > 0
            ->  Score is -Mate - Depth
            ;   D1 is Depth - 1,
                NA is -Beta, NB is -Alpha,
                negamax(NewPos, NewMask, D1, NA, NB, Val),
                Score is -Val
            )
        ),
        (   Score >= Beta
        ->  Final = Score
        ;   NewBest is max(Best, Score),
            NewAlpha is max(Alpha, Score),
            search_cols(Rest, Pos, Mask, Depth, NewAlpha, Beta, NewBest, Final)
        )
    ;   search_cols(Rest, Pos, Mask, Depth, Alpha, Beta, Best, Final)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ROOT CON TIEBREAK MÁS FUERTE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tiebreak MÁS fuerte basado en evaluación posicional
root_tiebreak_bonus(Col, Bonus) :-
    % Preferencia centro con valores significativos
    (   Col =:= 3 -> Bonus = 150      % Centro absoluto
    ;   Col =:= 2 -> Bonus = 100      % Adyacente izq
    ;   Col =:= 4 -> Bonus = 100      % Adyacente der
    ;   Col =:= 1 -> Bonus = 50       % Cerca centro izq
    ;   Col =:= 5 -> Bonus = 50       % Cerca centro der
    ;   Bonus = 0                      % Bordes
    ).

analyser(Plateau, Jeton, Scores) :-
    retractall(compteur_noeuds(_)),
    assertz(compteur_noeuds(0)),
    plateau_to_bitboard(Plateau, Jeton, Pos, Mask),
    profondeur_recherche(D),
    
    OppPos is Mask xor Pos,
    count_win_in1(OppPos, Mask, _, OppWinNowMask),
    
    findall(ScoreFinal, (
        between(0, 6, Col),
        eval_root(Pos, Mask, Col, D, OppWinNowMask, Score0),
        root_tiebreak_bonus(Col, TB),
        ScoreFinal is Score0 + TB
    ), Scores).

eval_root(Pos, Mask, Col, Depth, OppWinNowMask, Score) :-
    mate_value(Mate),
    (   \+ can_play(Mask, Col)
    ->  Score is -Mate - 999
    ;   make_move(Pos, Mask, Col, NewPos, NewMask, MoveBit),
        JustPlayedPos is NewMask xor NewPos,
        (   is_winning(JustPlayedPos)
        ->  Score is Mate
        ;   count_win_in1(NewPos, NewMask, OppWin1After, _),
            (   OppWin1After > 0
            ->  Score is -Mate
            ;   D1 is Depth - 1,
                negamax(NewPos, NewMask, D1, -Mate, Mate, Val),
                Base is -Val,
                % Bonus si bloqueamos amenaza inmediata
                (   (MoveBit /\ OppWinNowMask) =\= 0
                ->  Score is Base + 25000
                ;   Score is Base
                )
            )
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CONVERSION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plateau_to_bitboard(Cols, Jeton, Pos, Mask) :-
    conv_cols(Cols, 0, Jeton, 0, 0, Pos, Mask).

conv_cols([], _, _, P, M, P, M).
conv_cols([Col|R], Idx, Jeton, PAcc, MAcc, POut, MOut) :-
    conv_cells(Col, Idx, 0, Jeton, PAcc, MAcc, P1, M1),
    Idx1 is Idx + 1,
    conv_cols(R, Idx1, Jeton, P1, M1, POut, MOut).

conv_cells([], _, _, _, P, M, P, M).
conv_cells([C|R], CIdx, RIdx, Jeton, PAcc, MAcc, POut, MOut) :-
    (   is_token(C)
    ->  BitPos is CIdx * 7 + RIdx,
        Bit is 1 << BitPos,
        M1 is MAcc \/ Bit,
        (C == Jeton -> P1 is PAcc \/ Bit ; P1 = PAcc),
        R1 is RIdx + 1,
        conv_cells(R, CIdx, R1, Jeton, P1, M1, POut, MOut)
    ;   R1 is RIdx + 1,
        conv_cells(R, CIdx, R1, Jeton, PAcc, MAcc, POut, MOut)
    ).

is_token(x). is_token(o). is_token('x'). is_token('o').