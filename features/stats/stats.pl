:- consult('../../puissance4.pl').

:- dynamic res/5.

mean(NbCoup,Win,Mean) :- (not(NbCoup == 0), Mean is NbCoup/Win) ; (Mean is 0).
ia_type('aiMinMax').
ia_type('aiRand').
ia_type('aiV2').
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
    nl,
    run_against(IA1, Rest, NbMatch).


runStat(0, TypeR, TypeJ) :- 
    res(CoupJaune,CoupRouge,WinJaune,WinRouge,TotalMatch),
    mean(CoupJaune,WinJaune,MeanJ),
    mean(CoupRouge,WinRouge,MeanR),
    PercWinR is WinRouge/TotalMatch * 100,
    PercWinJ is WinJaune/TotalMatch * 100,
    writeln("---RESULTATS---"),
    write('Nombre de match : '), writeln(TotalMatch),
    write(TypeR), write(' :'), write(WinRouge), write(' victoires ('), write(PercWinR), write(' %) avec une moyenne de '), write(MeanR), writeln(' coup.'),
    write(TypeJ), write(' :'), write(WinJaune), write(' victoires ('), write(PercWinJ), write(' %)avec une moyenne de '), write(MeanJ), writeln(' coup.'),!.

initRunStat(NbMatch, TypeR, TypeJ) :- retractall(column(_,_,_)),
    retractall(res(_,_,_,_,_)),
    assert(res(0,0,0,0,NbMatch)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    initPlayerStat(PlayerR , TypeR, PlayerJ, TypeJ),
    playStat(0),
    NewNbMatch is NbMatch - 1,
    runStat(NewNbMatch, TypeR, TypeJ).

runStat(NbMatch, TypeR, TypeJ) :- not(NbMatch == 0),retractall(column(_,_,_)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    initPlayerStat(PlayerR , TypeR, PlayerJ, TypeJ),
    playStat(0),
    NewNbMatch is NbMatch - 1,
    runStat(NewNbMatch, TypeR, TypeJ).

initPlayerStat(PlayerR, TypeR, PlayerJ, TypeJ) :-
    retractall(playerR(_, _)),
    retractall(playerJ(_, _)),
    retractall(currentPlayer(_)),

    PlayerR = player('R', TypeR),
    assert(playerR('R', TypeR)),
    PlayerJ = player('Y', TypeJ),
    assert(playerJ('Y', TypeJ)),

    random(1,3, Num),
    (   Num =:= 1 ->
        assert(currentPlayer(PlayerJ))
    ;    
        assert(currentPlayer(PlayerR))
    ).

playStat(Count) :-
    NewCount is Count + 1,
    currentPlayer(Current),
    Current = player(Color,Type),
    choose_ai_move(Type, Move, Current),
    playMove(Move, Color, NewCol), 

    (   isOver(Color, Move) -> 
        (%write('Partie gagne en '),
        %write(NewCount),
        %writeln(' coup.'),
        res(CoupJaune,CoupRouge,WinJaune,WinRouge,TotalMatch),
        retract(res(CoupJaune,CoupRouge,WinJaune,WinRouge,TotalMatch)),
        ((Color == 'R', NewCoupRouge is CoupRouge + NewCount, NewWinRouge is WinRouge + 1, assert(res(CoupJaune,NewCoupRouge,WinJaune,NewWinRouge,TotalMatch)));
        (NewCoupJaune is CoupJaune + NewCount, NewWinJaune is WinJaune + 1, assert(res(NewCoupJaune,CoupRouge,NewWinJaune,WinRouge,TotalMatch)))),
        !) % on stoppe le jeu
    ;
        nextPlayer,
        playStat(NewCount)  % on continue de jouer
    ).

choose_ai_move('aiMinMax', Move, Current) :- ai(Move, Current).
choose_ai_move('aiRand',   Move, Current) :- aiV1(Move, Current).
choose_ai_move('aiV2',     Move, Current) :- aiV2(Move, Current).