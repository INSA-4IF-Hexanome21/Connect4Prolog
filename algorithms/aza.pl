:- consult('algorithms/router.pl').
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

ai(Move, player(Color, Strategy)) :-
    format('AI (~w) is thinking using strategy: ~w...~n', [Color, Strategy]),

    % Call the bridge module
    ia_choose_move(Color, Move, Strategy),

    format('AI plays column : ~w~n', [Move]).

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
    Current = player(Color,Type,Strategy), % Modification pour inclure la stratégie
    write('New turn for: '), writeln(Color),

    (   Type == 'ai' ->
        ai(Move,player(Color,Strategy)) % Appel l'IA pour un mouvement
    ;
        askPlayerMove(Move,Current) % Appel le joueur pour un mouvement
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
selectPlayerType(Type, Strategy) :- % Modification pour retourner la stratégie
    writeln('1 - ai  '),
    writeln('2 - player '),
    read(Entry),
    (   Entry =:= 1 ->
        Type = 'ai',
        selectAIStrategy(Strategy) % Si c'est une IA, choisir la stratégie
    ;   Entry =:= 2 ->
        Type = 'human',
        Strategy = 'none' % Les joueurs humains n'ont pas de stratégie
    ;   writeln('Invalid option.'), fail
    ).

% Nouveau prédicat pour choisir la stratégie de l'IA
selectAIStrategy(Strategy) :-
    writeln('Select AI Strategy:'),
    writeln('1 - random'),
    writeln('2 - minimax_v2'), % Assumer que ai_V2 est une stratégie minimax_v2
    read(Entry),
    (   Entry =:= 1 -> Strategy = 'random'
    ;   Entry =:= 2 -> Strategy = 'minimax_v2'
    ;   writeln('Invalid strategy option.'), fail
    ).

initBoard :-
    retractall(column(_,_,_)),
    length(EmptyCol, 6), maplist(=('e'), EmptyCol),
    forall(between(1,7,Idx),
        assert(column(Idx, EmptyCol,0))
    ),
    random(1, 3, Numero),
    writeln(Numero),
    % convertir_joueur(Numero, Joueur), % Garde l'original si pertinent
    true. % Ajouté pour que initBoard réussisse si convertir_joueur n'est pas défini

initPlayer(PlayerR, PlayerJ) :-
    writeln('--- Red Player (RED) ---'),
    selectPlayerType(TypeR, StrategyR), % Récupère le type et la stratégie
    nl, format('Red Player is ~w with strategy ~w~n', [TypeR, StrategyR]), % Affichage mis à jour

    writeln('--- Yellow Player (YELLOW) ---'),
    selectPlayerType(TypeJ, StrategyJ), % Récupère le type et la stratégie
    nl, format('Yellow Player is ~w with strategy ~w~n', [TypeJ, StrategyJ]), % Affichage mis à jour

    PlayerR = player('RED',TypeR,StrategyR), % Modification du terme joueur
    PlayerJ = player('YELLOW',TypeJ,StrategyJ). % Modification du terme joueur

initPlay(PlayerR, PlayerJ):-
    initBoard, % S'assurer que le plateau est initialisé
    play(PlayerJ, PlayerJ, PlayerR).