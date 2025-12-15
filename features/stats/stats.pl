:- consult('../../puissance4.pl').

:- dynamic res/5.

mean(NbCoup,Win,Mean) :- (not(NbCoup == 0), Mean is NbCoup/Win) ; (Mean is 0).

runStat(0) :- 
    res(CoupJaune,CoupRouge,WinJaune,WinRouge,TotalMatch),
    mean(CoupJaune,WinJaune,MeanJ),
    mean(CoupRouge,WinRouge,MeanR),
    PercWinR is WinRouge/TotalMatch * 100,
    PercWinJ is WinJaune/TotalMatch * 100,
    writeln("---RESULTATS---"),
    write('Nombre de match : '), writeln(TotalMatch),
    write('IA RAND : '), write(WinRouge), write(' victoires ('), write(PercWinR), write(' %) avec une moyenne de '), write(MeanR), writeln(' coup.'),
    write('IA V2 : '), write(WinJaune), write(' victoires ('), write(PercWinJ), write(' %)avec une moyenne de '), write(MeanJ), writeln(' coup.'),!.

initRunStat(NbMatch) :- retractall(column(_,_,_)),
    retractall(res(_,_,_,_,_)),
    assert(res(0,0,0,0,NbMatch)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    initPlayerStat(PlayerR, PlayerJ),
    displayBoard,
    playStat(0),
    NewNbMatch is NbMatch - 1,
    runStat(NewNbMatch).

runStat(NbMatch) :- not(NbMatch == 0),retractall(column(_,_,_)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    initPlayerStat(PlayerR, PlayerJ),
    displayBoard,
    playStat(0),
    NewNbMatch is NbMatch - 1,
    runStat(NewNbMatch).

initPlayerStat(PlayerR, PlayerJ) :-
    retractall(playerR(_, _)),
    retractall(playerJ(_, _)),
    retractall(currentPlayer(_)),

    PlayerR = player('R','aiRand'),
    assert(playerR('R', 'aiRand')),
    PlayerJ = player('Y','aiV2'),
    assert(playerJ('Y','aiV2')),
    write('Player J initialized as: '), writeln(PlayerJ),
    write('Player R initialized as: '), writeln(PlayerR),

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
    write('New turn for: '), writeln(Color),

    (   Type == 'aiMinMax' -> 
        ai(Move,Current) %Appel l'IA pour un mouvement
    ;
        Type == 'aiRand' -> 
        aiV1(Move,Current) %Appel l'IA pour un mouvement
    ;
        Type == 'aiV2' -> 
        aiV2(Move,Current) %Appel l'IA pour un mouvement
    ;
        askPlayerMove(Move,Current) %Appel le joueur pour un mouvement
    ),   

    
    playMove(Move, Color, NewCol),  
    displayBoard, 

    (   isOver(Color, Move) -> 
        (write('Partie gagne en '),
        write(NewCount),
        writeln(' coup.'),
        res(CoupJaune,CoupRouge,WinJaune,WinRouge,TotalMatch),
        retract(res(CoupJaune,CoupRouge,WinJaune,WinRouge,TotalMatch)),
        ((Color == 'R', NewCoupRouge is CoupRouge + NewCount, NewWinRouge is WinRouge + 1, assert(res(CoupJaune,NewCoupRouge,WinJaune,NewWinRouge,TotalMatch)));
        (NewCoupJaune is CoupJaune + NewCount, NewWinJaune is WinJaune + 1, assert(res(NewCoupJaune,CoupRouge,NewWinJaune,WinRouge,TotalMatch)))),
        !) % on stoppe le jeu
    ;
        nextPlayer,
        playStat(NewCount)  % on continue de jouer
    ).