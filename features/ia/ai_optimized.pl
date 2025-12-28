:- consult('features/ia/negamax_optimized.pl').

obtenir_colonne(N, D) :- column(N, D, _).

convertir_plateau(P) :-
    findall(C, (between(1,7,I), obtenir_colonne(I,B), clean(B,C)), P).

clean(B, C) :- include(\=('e'), B, S), maplist(tr, S, C).

tr('Y',x). tr('R',o). tr('YELLOW',x). tr('RED',o). tr(x,x). tr(o,o). tr('y',x). tr('r',o).

ia_op_choisir_coup(Couleur, Move) :-
    format(user_error, '[IA] Tour ~w~n', [Couleur]),
    tr(Couleur, J),
    convertir_plateau(P),
    analyser(P, J, Scores),
    format(user_error, '[IA] Scores: ~w~n', [Scores]),
    best_col(Scores, 0, -9999999, -1, BestIdx),
    Move is BestIdx + 1,
    format(user_error, '[IA] Joue colonne ~w~n~n', [Move]).

best_col([], _, _, B, B).
best_col([S|R], I, Max, BIdx, Final) :-
    I1 is I + 1,
    Col is I + 1,
    (   S > Max, column(Col, _, H), H < 6
    ->  best_col(R, I1, S, I, Final)
    ;   best_col(R, I1, Max, BIdx, Final)
    ).