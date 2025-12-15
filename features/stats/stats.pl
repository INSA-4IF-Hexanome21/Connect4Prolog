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
    res(TempsJaune,TempsRouge,CoupJaune,CoupRouge,WinJaune,WinRouge,TotalMatch),
    meanCoupParPartie(CoupRouge, TotalMatch, MeanCoupR),
    meanCoupParPartie(CoupJaune, TotalMatch, MeanCoupJ),
    meanTemps(TempsJaune,CoupJaune,MeanTempsJ),
    meanTemps(TempsRouge,CoupRouge,MeanTempsR),
    PercWinR is WinRouge/TotalMatch * 100,
    PercWinJ is WinJaune/TotalMatch * 100,
    
    writeln("---RESULTATS---"),
    write('Nombre de match : '), writeln(TotalMatch),
    write(TypeR), write(' :'), write(WinRouge), 
    write(' victoires ('), write(PercWinR), 
    write(' %) avec une moyenne de '), write(MeanCoupR), 
    write(' coups et un temps de reflexion de '),
    write(MeanTempsR), writeln(' micros/coup.'),

    write(TypeJ), write(' :'), write(WinJaune), 
    write(' victoires ('), write(PercWinJ), 
    write(' %) avec une moyenne de '), write(MeanCoupJ), 
    write(' coups et un temps de reflexion de '),
    write(MeanTempsJ), writeln(' micros/coup.'),!.

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

        (   Color == 'R'
        ->  NewCoupRouge is CR + PlayerMoves,
            NewWinRouge  is WR + 1,
            assert(res(TJ,TR,CJ,NewCoupRouge,WJ,NewWinRouge,TM))
        ;   NewCoupJaune is CJ + PlayerMoves,
            NewWinJaune  is WJ + 1,
            assert(res(TJ,TR,NewCoupJaune,CR,NewWinJaune,WR,TM))
        ),
        !
    ;   nextPlayer,
        playStat(NewCount)
    ).

choose_ai_move('aiMinMax', Move, Current) :- ai(Move, Current).
choose_ai_move('aiRand',   Move, Current) :- aiV1(Move, Current).
choose_ai_move('aiV2',     Move, Current) :- aiV2(Move, Current).
choose_ai_move('aiOp',     Move, Current) :- aiOp(Move, Current).