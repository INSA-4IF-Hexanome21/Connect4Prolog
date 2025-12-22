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
profondeur_recherche(12).

% 7 bits par colonne (6 lignes + 1 bit “sentinelle”).
% Ces valeurs correspondent à 7x6 avec sentinelle (bitboard standard).
bottom_mask(4432676798593).
board_mask(279258638311359).

move_order([3, 2, 4, 1, 5, 0, 6]).

mate_value(1000000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CŒUR DU BITBOARD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Calcule le bit correspondant à la case du bas d’une colonne donnée.
%Il sert à déclencher la gravité pour trouver où un pion va tomber dans cette colonne.
bottom_col(Col, Bit) :-
    Bit is 1 << (Col * 7).

%Construit un masque couvrant toutes les cases (et la sentinelle) d’une colonne.
%Il permet d’isoler une colonne et d’empêcher que les calculs débordent ailleurs.
column_mask(Col, Mask) :-
    % 7 bits (inclut la sentinelle à la ligne=6)
    Mask is 127 << (Col * 7).

%Donne le bit de la case jouable la plus haute d’une colonne.
%Il sert à savoir instantanément si une colonne est pleine ou non.
top_mask_playable(Col, Bit) :-
    % Ligne supérieure jouable : ligne = 5
    Bit is 1 << (Col * 7 + 5).

%Teste si la colonne est jouable en vérifiant que sa case haute est vide.
%Il évite de simuler des coups illégaux sans parcourir la colonne.
can_play(Mask, Col) :-
    between(0, 6, Col),
    top_mask_playable(Col, Top),
    (Mask /\ Top) =:= 0.

%Calcule exactement la case où tombe le pion dans une colonne donnée.
%C’est le cœur du moteur : il simule la gravité en O(1) avec une addition binaire.
move_bit(Mask, Col, MoveBit) :-
    bottom_col(Col, Bottom),
    column_mask(Col, ColMask),
    MoveBit is (Mask + Bottom) /\ ColMask.

%Applique un coup en ajoutant le pion au plateau et en changeant le joueur actif.
%Il met à jour l’état du jeu sans copier de plateau ni utiliser de conditions.
make_move(Pos, Mask, Col, NewPos, NewMask, MoveBit) :-
    move_bit(Mask, Col, MoveBit),
    NewMask is Mask \/ MoveBit,
    % NewPos = jetons du joueur qui doit jouer (adversaire)
    NewPos is Pos xor Mask.

%Calcule toutes les cases jouables du plateau en une seule opération.
%Il sert aux heuristiques et à la détection rapide des menaces.
playable_mask(Mask, Playable) :-
    bottom_mask(Bottom),
    board_mask(Board),
    % Cases jouables par gravité (prochain espace de chaque colonne)
    Playable is (Mask + Bottom) /\ Board.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DÉTECTION DE VICTOIRE (O(1))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Teste si une position contient un alignement gagnant dans une direction donnée.
%Il permet de détecter une victoire en temps constant uniquement avec des opérations bit-à-bit.
is_winning(Pos) :-
    % Horizontal (décalage 7)
    M1 is Pos /\ (Pos >> 7),
    (M1 /\ (M1 >> 14)) =\= 0,
    !.
is_winning(Pos) :-
    % Vertical (décalage 1)
    M2 is Pos /\ (Pos >> 1),
    (M2 /\ (M2 >> 2)) =\= 0,
    !.
is_winning(Pos) :-
    % Diagonale \
    M3 is Pos /\ (Pos >> 6),
    (M3 /\ (M3 >> 12)) =\= 0,
    !.
is_winning(Pos) :-
    % Diagonale /
    M4 is Pos /\ (Pos >> 8),
    (M4 /\ (M4 >> 16)) =\= 0,
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% POPCOUNT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

popcount(0, 0) :- !.
popcount(N, C) :-
    N1 is N /\ (N - 1),
    popcount(N1, C1),
    C is C1 + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LIGNES DE 4 (69 masques) pour heuristique
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lines(Lines) :-
    lines_cache(Lines),
    !.
lines(Lines) :-
    findall(M, line_mask(M), Lines),
    retractall(lines_cache(_)),
    assertz(lines_cache(Lines)).

line_mask(Mask) :-
    % Horizontal : dc=1 dr=0
    between(0, 3, C0),
    between(0, 5, R0),
    line_from(C0, R0, 1, 0, Mask).
line_mask(Mask) :-
    % Vertical : dc=0 dr=1
    between(0, 6, C0),
    between(0, 2, R0),
    line_from(C0, R0, 0, 1, Mask).
line_mask(Mask) :-
    % Diagonale / : dc=1 dr=1
    between(0, 3, C0),
    between(0, 2, R0),
    line_from(C0, R0, 1, 1, Mask).
line_mask(Mask) :-
    % Diagonale \ : dc=1 dr=-1
    between(0, 3, C0),
    between(3, 5, R0),
    line_from(C0, R0, 1, -1, Mask).

line_from(C0, R0, DC, DR, Mask) :-
    bit_at(C0, R0, B0),
    C1 is C0 + DC,
    R1 is R0 + DR,
    bit_at(C1, R1, B1),
    C2 is C0 + 2 * DC,
    R2 is R0 + 2 * DR,
    bit_at(C2, R2, B2),
    C3 is C0 + 3 * DC,
    R3 is R0 + 3 * DR,
    bit_at(C3, R3, B3),
    Mask is B0 \/ B1 \/ B2 \/ B3.

bit_at(Col, Row, Bit) :-
    Bit is 1 << (Col * 7 + Row).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HEURISTIQUES “INTELLIGENTES” 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Menace = fenêtre de 4 avec 3 de tes jetons + 1 vide, et la case vide est jouable (gravité).
count_threat3_playable(Pos, OppPos, Mask, Count) :-
    lines(Lines),
    playable_mask(Mask, Playable),
    board_mask(Board),
    Empty is Board xor Mask,
    count_threat3_playable_(Lines, Pos, OppPos, Empty, Playable, 0, Count).

count_threat3_playable_([], _, _, _, _, Acc, Acc).
count_threat3_playable_([L | Rest], Pos, OppPos, Empty, Playable, Acc, Count) :-
    OpIn is L /\ OppPos,
    (   OpIn =\= 0
    ->  Acc1 = Acc
    ;   MyIn is L /\ Pos,
        popcount(MyIn, MyC),
        (   MyC =:= 3
        ->  Vac is L /\ Empty,
            popcount(Vac, VacC),
            (   VacC =:= 1,
                (Vac /\ Playable) =\= 0
            ->  Acc1 is Acc + 1
            ;   Acc1 = Acc
            )
        ;   Acc1 = Acc
        )
    ),
    count_threat3_playable_(Rest, Pos, OppPos, Empty, Playable, Acc1, Count).

% Évaluation par lignes ouvertes (2-en-ligne, 3-en-ligne sans blocage).
line_weight(0, 0).
line_weight(1, 1).
line_weight(2, 8).
line_weight(3, 60).
line_weight(4, 0).

open_lines_score(Pos, OppPos, Score) :-
    lines(Lines),
    open_lines_score_(Lines, Pos, OppPos, 0, Score).

open_lines_score_([], _, _, Acc, Acc).
open_lines_score_([L | Rest], Pos, OppPos, Acc, Score) :-
    MyIn is L /\ Pos,
    OpIn is L /\ OppPos,
    (   OpIn =:= 0
    ->  popcount(MyIn, N),
        line_weight(N, W),
        Acc1 is Acc + W
    ;   Acc1 = Acc
    ),
    (   MyIn =:= 0
    ->  popcount(OpIn, N2),
        line_weight(N2, W2),
        Acc2 is Acc1 - W2
    ;   Acc2 = Acc1
    ),
    open_lines_score_(Rest, Pos, OppPos, Acc2, Score).

% Centre (comme ton algorithme 1 : colonnes 2,3,4)
center_mask(17044586496).

center_score(Pos, OppPos, Score) :-
    center_mask(CM),
    popcount(Pos /\ CM, MC),
    popcount(OppPos /\ CM, OC),
    Score is (MC - OC) * 6.

% Compte “gagner en 1” (menaces immédiates réelles) en simulant des colonnes.
count_win_in1(Pos, Mask, Count, WinMoveMask) :-
    move_order(Cols),
    count_win_in1_(Cols, Pos, Mask, 0, 0, Count, WinMoveMask).

count_win_in1_([], _, _, AccC, AccM, AccC, AccM).
count_win_in1_([Col | Rest], Pos, Mask, AccC, AccM, Count, WinMoveMask) :-
    (   can_play(Mask, Col)
    ->  make_move(Pos, Mask, Col, NewPos, NewMask, MoveBit),
        JustPlayedPos is NewMask xor NewPos,
        (   is_winning(JustPlayedPos)
        ->  AccC1 is AccC + 1,
            AccM1 is AccM \/ MoveBit
        ;   AccC1 = AccC,
            AccM1 = AccM
        ),
        count_win_in1_(Rest, Pos, Mask, AccC1, AccM1, Count, WinMoveMask)
    ;   count_win_in1_(Rest, Pos, Mask, AccC, AccM, Count, WinMoveMask)
    ).

eval_position(Pos, Mask, Score) :-
    OppPos is Mask xor Pos,

    % Menaces immédiates (gagner en 1)
    count_win_in1(Pos, Mask, MyWin1, _),
    count_win_in1(OppPos, Mask, OpWin1, _),

    % Menaces de type “3 + case jouable”
    count_threat3_playable(Pos, OppPos, Mask, MyThreat3),
    count_threat3_playable(OppPos, Pos, Mask, OpThreat3),

    % Positionnel
    center_score(Pos, OppPos, CenterS),
    open_lines_score(Pos, OppPos, LinesS),

    % Poids : défendre un Win1 de l'adversaire est plus urgent
    (   MyWin1 >= 2
    ->  ForkBonus = 5000
    ;   ForkBonus = 0
    ),
    Score is MyWin1 * 9000
          - OpWin1 * 12000
          + MyThreat3 * 300
          - OpThreat3 * 450
          + CenterS
          + LinesS
          + ForkBonus.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NEGAMAX + ALPHA-BETA + FILTRE NE PAS PERDRE EN 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inc_node :-
    (   retract(compteur_noeuds(N))
    ->  N1 is N + 1,
        assertz(compteur_noeuds(N1))
    ;   assertz(compteur_noeuds(1))
    ).

negamax(Pos, Mask, Depth, Alpha, Beta, Score) :-
    inc_node,
    OppPos is Mask xor Pos,
    mate_value(Mate),
    (   is_winning(OppPos)
    ->  % Le joueur précédent a déjà gagné => état perdu
        Score is -Mate - Depth
    ;   board_mask(Board),
        Mask =:= Board
    ->  Score is 0
    ;   Depth =:= 0
    ->  eval_position(Pos, Mask, Score)
    ;   % Si je gagne en 1, pas besoin de chercher plus
        count_win_in1(Pos, Mask, MyWin1, _),
        MyWin1 > 0
    ->  Score is Mate + Depth
    ;   search_moves(Pos, Mask, Depth, Alpha, Beta, -Mate, Score)
    ).

search_moves(Pos, Mask, Depth, Alpha, Beta, Best, FinalScore) :-
    move_order(Cols),
    search_cols(Cols, Pos, Mask, Depth, Alpha, Beta, Best, FinalScore).

search_cols([], _, _, _, _, _, Best, Best).
search_cols([Col | Rest], Pos, Mask, Depth, Alpha, Beta, Best, Final) :-
    mate_value(Mate),
    (   can_play(Mask, Col)
    ->  make_move(Pos, Mask, Col, NewPos, NewMask, _MoveBit),
        JustPlayedPos is NewMask xor NewPos,
        (   is_winning(JustPlayedPos)
        ->  Score is Mate + Depth
        ;   % Filtre : si après mon coup l'adversaire a un gain-en-1 => mauvais coup
            count_win_in1(NewPos, NewMask, OppWin1After, _),
            (   OppWin1After > 0
            ->  Score is -Mate - Depth
            ;   D1 is Depth - 1,
                NA is -Beta,
                NB is -Alpha,
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
%% RACINE analyser
%% de plus brise les égalités vers le centre
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

root_tiebreak_bonus(0, 0).
root_tiebreak_bonus(1, 1).
root_tiebreak_bonus(2, 2).
root_tiebreak_bonus(3, 3).
root_tiebreak_bonus(4, 2).
root_tiebreak_bonus(5, 1).
root_tiebreak_bonus(6, 0).

analyser(Plateau, Jeton, Scores) :-
    retractall(compteur_noeuds(_)),
    assertz(compteur_noeuds(0)),
    plateau_to_bitboard(Plateau, Jeton, Pos, Mask),
    profondeur_recherche(D),

    % Menaces immédiates de l'adversaire dans la position actuelle (si l'adversaire jouait maintenant)
    OppPos is Mask xor Pos,
    count_win_in1(OppPos, Mask, _, OppWinNowMask),

    findall(ScoreFinal,
            (between(0, 6, Col),
             eval_root(Pos, Mask, Col, D, OppWinNowMask, Score0),
             root_tiebreak_bonus(Col, TB),
             ScoreFinal is Score0 + TB),
            Scores).

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
                (   (MoveBit /\ OppWinNowMask) =\= 0
                ->  Score is Base + 20000
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
conv_cols([Col | R], Idx, Jeton, PAcc, MAcc, POut, MOut) :-
    conv_cells(Col, Idx, 0, Jeton, PAcc, MAcc, P1, M1),
    Idx1 is Idx + 1,
    conv_cols(R, Idx1, Jeton, P1, M1, POut, MOut).

conv_cells([], _, _, _, P, M, P, M).
conv_cells([C | R], CIdx, RIdx, Jeton, PAcc, MAcc, POut, MOut) :-
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

is_token(x).
is_token(o).
is_token('x').
is_token('o').