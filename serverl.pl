:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).
:- use_module(library(random)).
:- use_module(library(lists)).

% ====== GAME ENGINE ======
:- consult('features/ia/ai.pl').
:- consult('features/affichage/affichage.pl').
:- consult('features/coup/coup.pl').
:- consult('features/fin/fin.pl').
:- consult('features/ia/ai_random.pl').
:- consult('features/ia/ai_V2.pl').
:- consult('features/joueur/joueur.pl').
:- consult('features/ia/ai_optimized.pl').

% ====== DYNAMIC STATE ======
:- dynamic column/3.
:- dynamic currentPlayer/1.
:- dynamic playerR/2.
:- dynamic playerJ/2.

% ====== SERVER ======
start_server :-
    http_server(http_dispatch, [port(8080)]),
    writeln('% Server running on http://localhost:8080'),
    thread_get_message(_).

% ====== STATIC FILES ======
:- http_handler(root(.), serve_static, [prefix]).
serve_static(Req) :-
    member(path(Path), Req),
    ( Path = '/' -> http_reply_file('view/index.html', [], [])
    ; http_reply_from_files('view', [], Req)
    ).

:- http_handler(root('favicon.ico'), serve_favicon, []).
serve_favicon(_) :- format('Content-type: image/x-icon~n~n').

% ====== API ======
:- http_handler(root(api/init_game), h_init_game, [methods([post])]).
h_init_game(Request) :-
    catch(
        (
            http_read_json_dict(Request, Data),
            ( _{red:TypeR, yellow:TypeY} :< Data -> true ; throw(error(missing_field)) ),
            init_board_api,
            init_players_api(TypeR, TypeY, Starting),
            get_board_json(Board),
            reply_json(json{
                board: Board,
                currentPlayer: Starting,
                status: playing
            })
        ),
        E,
        (
            print_message(error, E),
            make_empty_board(Board),
            reply_json(json{error:'Init failed', board:Board, status:'error'})
        )
    ).

:- http_handler(root(api/play_move), h_play_move, [methods([post])]).
h_play_move(Request) :-
    catch(
        (
            http_read_json_dict(Request, Data),
            Col = Data.column,
            currentPlayer(Color),
            valid_column(Col),
            playMove(Col, Color, _),
            get_board_json(Board),
            ( isOver(Color, Col) -> Status = finished, Winner = Color ; next_player_api, Status = playing, Winner = none ),
            currentPlayer(Next),
            reply_json(json{
                board: Board,
                status: Status,
                winner: Winner,
                nextPlayer: Next
            })
        ),
        E,
        (
            print_message(error, E),
            make_empty_board(Board),
            reply_json(json{error:'Invalid move', board:Board, status:'error'})
        )
    ).

h_ia_move(Request) :-
    catch(
        (
            % Step 1: get current player
            (currentPlayer(Color) -> format('Current player: ~w~n', [Color]) ; throw(error(no_current_player))),
            
            % Step 2: get player type
            (get_player_type(Color, Type) -> format('Player type: ~w~n', [Type]) ; throw(error(no_player_type))),
            
            % Step 3: choose IA move
            (ia_choose_move(Type, Color, Move) -> format('IA chooses: ~w~n', [Move]) ; throw(error(ia_failed))),
            
            % Step 4: play move
            (playMove(Move, Color, _) -> format('Played move: ~w~n', [Move]) ; throw(error(play_failed))),
            
            % Step 5: get board
            get_board_json(Board),
            
            % Step 6: check if game over
            ( isOver(Color, Move) -> Status = finished, Winner = Color ; next_player_api, Status = playing, Winner = none ),
            currentPlayer(Next),
            
            % Step 7: reply JSON
            reply_json(json{
                column: Move,
                board: Board,
                status: Status,
                winner: Winner,
                nextPlayer: Next,
                iaType: Type,
                player: Color
            })
        ),
        E,
        (
            print_message(error,E),
            make_empty_board(Board),
            reply_json(json{error:'IA failed', board:Board, status:'error'})
        )
    ).



% ====== GAME LOGIC ======
init_board_api :-
    retractall(column(_,_,_)),
    forall(between(1,7,C),
        assert(column(C, ['e','e','e','e','e','e'], 0))
    ).

init_players_api(TypeR, TypeY, Starting) :-
    retractall(playerR(_,_)),
    retractall(playerJ(_,_)),
    retractall(currentPlayer(_)),
    assert(playerR('r', TypeR)),
    assert(playerJ('y', TypeY)),
    random_between(0,1,X),
    ( X = 0 -> assert(currentPlayer('r')), Starting = 'r'
    ; assert(currentPlayer('y')), Starting = 'y'
    ).

next_player_api :-
    retract(currentPlayer(C)),
    ( C = 'r' -> N = 'y' ; N = 'r' ),
    assert(currentPlayer(N)).

get_player_type('r', Type) :- playerR('r', Type).
get_player_type('y', Type) :- playerJ('y', Type).

valid_column(Col) :-
    integer(Col),
    between(1,7,Col),
    column(Col, _, Filled),
    Filled < 6.

% ====== IA ======
ia_choose_move(Type, Color, Move) :-
    catch(call_ai(Type, Color, M), E, (print_message(error,E), fail)),
    ( valid_column(M) -> Move = M
    ; findall(C, valid_column(C), ValidCols),
      random_member(Move, ValidCols)
    ).

call_ai('aiRand', Color, Move) :- aiV1(Move, player(Color,_)).
call_ai('aiV2', Color, Move) :- aiV2(Move, player(Color,_)).
call_ai('aiMinMax', Color, Move) :- ai(Move, player(Color,_)).
call_ai('aiOp', Color, Move) :- aiOp(Move, player(Color,_)).

% ====== BOARD JSON ======
get_board_json(Board) :-
    findall(ColData, (between(1,7,I), column(I, ColData, _)), Cols),
    maplist(reverse, Cols, RevCols),
    transpose_matrix(RevCols, Board).

transpose_matrix([], []).
transpose_matrix([[]|_], []) :- !.
transpose_matrix(Matrix, [Row|Rows]) :-
    findall(E, (member(Col, Matrix), nth1(1, Col, E)), Row),
    maplist(remove_head, Matrix, Rest),
    transpose_matrix(Rest, Rows).

remove_head([_|T], T).

make_empty_board(Board) :-
    length(Row, 7), maplist(=(' '), Row),
    length(Board, 6), maplist(=(Row), Board).

convert_cell('e', ' ').
convert_cell('r', 'r').
convert_cell('y', 'y').
