:- consult('features/ia/ai.pl').
:- consult('features/affichage/affichage.pl').
:- consult('features/coup/coup.pl').
:- consult('features/fin/fin.pl').
:- consult('features/ia/ai_random.pl').
:- consult('features/ia/ai_V2.pl').
:- consult('features/joueur/joueur.pl').
:- consult('features/ia/ai_optimized.pl').

:- dynamic column/3. % column(Col, ColData, LastPos)
:- dynamic player/2.
:- dynamic playerJ/2.
:- dynamic playerR/2.
:- dynamic currentPlayer/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------------   IA   -------------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Le Player est une structure player(Color, Type)
ai(Move, player(Color, _)) :- 
    %writeln('AI is thinking...'),
    ia_choisir_coup(Color, Move),
    %write('AI plays column : '), writeln(Move).
    !.

aiOp(Move, player(Color, _)) :-
    writeln('AI optimized is thinking...'),
    ia_op_choisir_coup(Color, Move),
    write('AI optimized plays column : '), writeln(Move).

askPlayerMove(Move,_) :-
    repeat,
    writeln('Where do you want to play ? '),
    read(Entry),
    (   integer(Entry), Entry>0, Entry<8, column(Entry,_,LastPos), not(LastPos == 6) ->
        Move = Entry
    ;
        writeln('Invalid option.'), fail
    ),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   BOUCLE DE JEU   -------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play :-
    currentPlayer(Current),
    Current = player(Color,Type),
    write('New turn for: '), writeln(Color),
    choose_ai_move(Type, Move, Current),
    playMove(Move, Color, NewCol),  
    displayBoard, 

    (   isOver(Color, Move) -> 
        write(Color), writeln(' has won the match !'),
        true % on stoppe le jeu
    ;
        isTie ->
        writeln('It\'s a tie !')
    ;
        nextPlayer,
        play  % on continue de jouer
    ).
    
choose_ai_move('aiMinMax', Move, Current) :- ai(Move, Current).
choose_ai_move('aiRand',   Move, Current) :- aiV1(Move, Current).
choose_ai_move('aiV2',     Move, Current) :- aiV2(Move, Current).
choose_ai_move('human',    Move, Current) :- askPlayerMove(Move, Current).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   INITIALISATION   ------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
selectPlayerType(Type) :-
    writeln('1 - aiRand  '),
    writeln('2 - aiV2  '),
    writeln('3 - aiMinMax  '),
    writeln('4 - player '),
    writeln('5 - aiOptimized  '),
    read(Entry),
    (   Entry =:= 1 -> Type = 'aiRand'
    ;   Entry =:= 2 -> Type = 'aiV2'
    ;   Entry =:= 3 -> Type = 'aiMinMax'
    ;   Entry =:= 4 -> Type = 'human'
    ;   Entry =:= 5 -> Type = 'aiOp'
    ;   writeln('Invalid option.'), fail
    ).

initBoard :-
    retractall(column(_,_,_)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    initPlayer(PlayerR, PlayerJ),
    displayBoard,
    play.

initPlayer(PlayerR, PlayerJ) :-
    retractall(playerR(_, _)),
    retractall(playerJ(_, _)),
    retractall(currentPlayer(_)),
    writeln('--- Red Player (R) ---'),
    selectPlayerType(TypeR),
    nl, write('Red Player is '), writeln(TypeR),

    writeln('--- Yellow Player (Y) ---'),
    selectPlayerType(TypeJ),
    nl, write('Yellow Player is '), writeln(TypeJ),

    PlayerR = player('R',TypeR),
    assert(playerR('R', TypeR)),
    PlayerJ = player('Y',TypeJ),
    assert(playerJ('Y',TypeJ)),
    write('Player J initialized as: '), writeln(PlayerJ),
    write('Player R initialized as: '), writeln(PlayerR),

    random(1,3, Num),
    (   Num =:= 1 ->
        assert(currentPlayer(PlayerJ)),
        write(PlayerJ), write(' starts')
    ;    
        assert(currentPlayer(PlayerR)),
        write(PlayerR), write(' starts')
    ).


