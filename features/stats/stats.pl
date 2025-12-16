:- consult('../../puissance4.pl').

:- dynamic res/7.
:- dynamic playerStat/3.

% meanCoup(NbCoup,Win,Mean) :- (not(NbCoup == 0), Mean is NbCoup/Win) ; (Mean is 0).
meanCoupParPartie(NbCoup, TotalMatch, Mean) :-
    ( TotalMatch > 0 ->
        Mean is NbCoup / TotalMatch
    ;
        Mean is 0
    ).
meanTemps(Temps, NbCoup, Mean) :-
    (   NbCoup > 0
    ->  Mean is Temps / NbCoup
    ;   Mean is 0
    ).
ia_type('aiMinMax').
ia_type('aiRand').
ia_type('aiV2').
ia_type('aiOp').
all_ias(List) :-
    findall(IA, ia_type(IA), List).

run_all_stats(NbMatch) :-
    findall(IA, ia_type(IA), IAs),
    run_pairs(IAs, NbMatch).

run_pairs([], _).
run_pairs([_], _).
run_pairs([IA1|Rest], NbMatch) :-
    run_against(IA1, Rest, NbMatch),
    run_pairs(Rest, NbMatch).

run_against(_, [], _).
run_against(IA1, [IA2|Rest], NbMatch) :-
    writeln('=============================='),
    write('MATCHUP : '), write(IA1), write(' VS '), writeln(IA2),
    writeln('=============================='),
    initRunStat(NbMatch, IA1, IA2),
    % also run the inverse matchup so each IA plays as PlayerJ (first)
    writeln('=============================='),
    write('MATCHUP (inverse) : '), write(IA2), write(' VS '), writeln(IA1),
    writeln('=============================='),
    initRunStat(NbMatch, IA2, IA1),
    nl,
    run_against(IA1, Rest, NbMatch).


runStat(0, TypeR, TypeJ) :- 
    res(TempsJaune, TempsRouge, CoupJaune, CoupRouge, WinJaune, WinRouge, TotalMatch),
    print_results(TypeR,TypeJ, WinRouge, WinJaune, CoupRouge, CoupJaune, TempsRouge, TempsJaune, TotalMatch).

print_results(Type1, Type2, Win1, Win2, Coup1, Coup2, Temps1, Temps2, TotalMatch) :-
    Perc1 is (Win1 / TotalMatch) * 100,
    Perc2 is (Win2 / TotalMatch) * 100,
    MeanCoup1 is Coup1 / TotalMatch,
    MeanCoup2 is Coup2 / TotalMatch,
    MeanTemps1Ms is Temps1 / TotalMatch / 1000,  % µs → ms
    MeanTemps2Ms is Temps2 / TotalMatch / 1000,
    
    format('--- RESULTATS ---~n', []),
    format('Nombre de parties : ~w~n~n', [TotalMatch]),
    writeln('Joueur jaune premier a jouer'),
    
    format('~w (R) :~n ', [Type1]),
    format('  Victoires : ~w (~2f %)~n', [Win1, Perc1]),
    format('  Moyenne coups / partie : ~2f~n', [MeanCoup1]),
    format('  Temps moyen / coup : ~2f ms~n~n', [MeanTemps1Ms]),
    
    format('~w (J) :~n ', [Type2]),
    format('  Victoires : ~w (~2f %)~n', [Win2, Perc2]),
    format('  Moyenne coups / partie : ~2f~n', [MeanCoup2]),
    format('  Temps moyen / coup : ~2f ms~n~n', [MeanTemps2Ms]).


initRunStat(NbMatch, TypeR, TypeJ) :- retractall(column(_,_,_)),
    retractall(res(_,_,_,_,_,_,_)),
    assert(res(0,0,0,0,0,0,NbMatch)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    initPlayerStat(PlayerR , TypeR, PlayerJ, TypeJ),
    playStat(0),
    NewNbMatch is NbMatch - 1,
    write('Avancement evaluation : '), write(NewNbMatch), writeln(' matches restants'),
    runStat(NewNbMatch, TypeR, TypeJ).

runStat(NbMatch, TypeR, TypeJ) :- not(NbMatch == 0),retractall(column(_,_,_)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    initPlayerStat(PlayerR , TypeR, PlayerJ, TypeJ),
    playStat(0),
    NewNbMatch is NbMatch - 1,
    write('Avancement evaluation : '), write(NewNbMatch), writeln(' matches restants'),
    runStat(NewNbMatch, TypeR, TypeJ).

initPlayerStat(PlayerR, TypeR, PlayerJ, TypeJ) :-
    retractall(playerR(_, _)),
    retractall(playerJ(_, _)),
    retractall(currentPlayer(_)),

    PlayerR = player('R', TypeR),
    assert(playerR('R', TypeR)),
    PlayerJ = player('Y', TypeJ),
    assert(playerJ('Y', TypeJ)),

    assert(currentPlayer(PlayerJ)).

timed_ai_move(Type, Current, Move, Time) :-
    get_time(T0),
    choose_ai_move(Type, Move, Current),
    get_time(T1),
    Time is (T1 - T0) * 1_000_000.

playStat(Count) :-
    NewCount is Count + 1,
    currentPlayer(Current),
    Current = player(Color,Type),

    timed_ai_move(Type, Current, Move, Time),

    % --- accumulation du temps a chaque coup ---
    res(TempsJaune,TempsRouge,CoupJaune,CoupRouge,WinJaune,WinRouge,TotalMatch),
    retract(res(TempsJaune,TempsRouge,CoupJaune,CoupRouge,WinJaune,WinRouge,TotalMatch)),
    (   Color == 'R'
    ->  NewTempsRouge is TempsRouge + Time,
        assert(res(TempsJaune,NewTempsRouge,CoupJaune,CoupRouge,WinJaune,WinRouge,TotalMatch))
    ;   NewTempsJaune is TempsJaune + Time,
        assert(res(NewTempsJaune,TempsRouge,CoupJaune,CoupRouge,WinJaune,WinRouge,TotalMatch))
    ),

    playMove(Move, Color, _NewCol),

    (   isOver(Color, Move)
    ->  % --- fin de partie : comptage des coups et victoires ---
        res(TJ,TR,CJ,CR,WJ,WR,TM),
        retract(res(TJ,TR,CJ,CR,WJ,WR,TM)),

        PlayerMoves is (NewCount + 1) // 2,
        OppMoves is NewCount - PlayerMoves,

        (   Color == 'R'
        ->  % Rouge just won: add Rouge moves and opponent (Jaune) moves, increment Rouge wins
            NewCoupRouge is CR + PlayerMoves,
            NewCoupJaune is CJ + OppMoves,
            NewWinRouge  is WR + 1,
            assert(res(TJ,TR,NewCoupJaune,NewCoupRouge,WJ,NewWinRouge,TM))
        ;   % Jaune just won: add Jaune moves and opponent (Rouge) moves, increment Jaune wins
            NewCoupJaune is CJ + PlayerMoves,
            NewCoupRouge is CR + OppMoves,
            NewWinJaune  is WJ + 1,
            assert(res(TJ,TR,NewCoupJaune,NewCoupRouge,NewWinJaune,WR,TM))
        ),
        !
    ;   nextPlayer,
        playStat(NewCount)
    ).

choose_ai_move('aiMinMax', Move, Current) :- ai(Move, Current).
choose_ai_move('aiRand',   Move, Current) :- aiV1(Move, Current).
choose_ai_move('aiV2',     Move, Current) :- aiV2(Move, Current).
choose_ai_move('aiOp',     Move, Current) :- aiOp(Move, Current).