:- consult('ai.pl').
:- consult('features/affichage/affichage.pl').
:- consult('features/coup/coup.pl').
:- consult('features/fin/fin.pl').
:- consult('features/ia/ai_random.pl').
:- consult('features/ia/ai_V2.pl').
:- consult('features/joueur/joueur.pl').

:- dynamic column/3. % column(Col, ColData, LastPos)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------------   IA   -------------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Le Player est une structure player(Color, Type)
ai(Move, player(Color, _)) :- 
    writeln('AI is thinking...'),
    ia_choisir_coup(Color, Move),
    write('AI plays column : '), writeln(Move).

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

play(Current, PlayerJ, PlayerR) :-
    Current = player(Color,Type),
    write('New turn for: '), writeln(Color),

    (   Type == 'ai' -> 
        ai(Move,Current) %Appel l'IA pour un mouvement
    ;
        askPlayerMove(Move,Current) %Appel le joueur pour un mouvement
    ),   

    displayBoard,
    playMove(Move, Color, NewCol),   

    (   isOver(Color, Move) -> 
        true % on stoppe le jeu
    ;
        nextPlayer(Current,PlayerJ, PlayerR, NextPlayer),
        play(NextPlayer, PlayerJ, PlayerR)  % on continue de jouer
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   INITIALISATION   ------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
selectPlayerType(Type) :-
    writeln('1 - ai  '),
    writeln('2 - player '),
    read(Entry),
    (   Entry =:= 1 -> Type = 'ai'
    ;   Entry =:= 2 -> Type = 'human'
    ;   writeln('Invalid option.'), fail
    ).

initBoard :-
    retractall(column(_,_,_)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    random(1, 3, Numero),
    writeln(Numero),
    convertir_joueur(Numero, Joueur),

initPlayer(PlayerR, PlayerJ) :-
    writeln('--- Red Player (RED) ---'),
    selectPlayerType(TypeR),
    nl, write('Red Player is '), writeln(TypeR),

    writeln('--- Yellow Player (YELLOW) ---'),
    selectPlayerType(TypeJ),
    nl, write('Yellow Player is '), writeln(TypeJ),

    PlayerR = player('RED',TypeR),
    PlayerJ = player('YELLOW',TypeJ).

initPlay(PlayerR, PlayerJ):- 
    play(PlayerJ, PlayerJ, PlayerR).

