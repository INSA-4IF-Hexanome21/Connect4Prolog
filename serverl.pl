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
    format(user_error, 'DEBUG: play_move handler called~n', []),
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

:- http_handler(root(api/ia_move), h_ia_move, [methods([post])]).
h_ia_move(_Request) :-
    format(user_error, 'DEBUG: ia_move handler called~n', []),
    catch(
        ( 
            http_read_json_dict(_Request, Data),
            % Ensure we have a current player
            ( currentPlayer(Color) -> true ; (
                make_empty_board(Board),
                reply_json(json{error:'no_current_player', board:Board, status:'error'}),
                !, fail
            ) ),

            % Ensure the player's type is known
            ( get_player_type(Color, Type) -> true ; (
                make_empty_board(Board),
                reply_json(json{error:'no_player_type', board:Board, status:'error'}),
                !, fail
            ) ),

            % Choose IA move
            ( ia_choose_move(Type, Color, Move) -> true ; (
                format(user_error, 'IA choose move failed: type=~w color=~w move=~w ~n', [Type, Color, Move]),
                make_empty_board(Board),
                reply_json(json{error:'ia_choose_failed', board:Board, status:'error'}),
                !, fail
            ) ),

            % Play the move
            ( playMove(Move, Color, _) -> true ; (
                format(user_error, 'playMove failed: move=~w color=~w~n', [Move, Color]),
                make_empty_board(Board),
                reply_json(json{error:'play_failed', board:Board, status:'error'}),
                !, fail
            ) ),

            % Build and send normal response
            format(user_error, 'IA move played successfully~n', []),
            get_board_json(Board),
            format(user_error, 'column: ~w~n', [Move]),
            format(user_error, 'board: ~w~n', [Board]),

            ( isOver(Color, Move) -> 
                Status = finished, Winner = Color 
            ; 
                next_player_api, Status = playing, Winner = none 
            ),
            currentPlayer(Next),
            
            format(user_error, 'status: ~w~n', [Status]),
            format(user_error, 'winner: ~w~n', [Winner]),
            format(user_error, 'nextPlayer: ~w~n', [Next]),
            
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
            message_to_string(E, Msg),
            format(user_error, 'IA handler exception: ~w~n', [Msg]),
            make_empty_board(Board),
            reply_json(json{error:'IA exception', detail:Msg, board:Board, status:'error'})
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
    format(user_error, 'DEBUG: ia_choose_move called with Type=~w, Color=~w~n', [Type, Color]),
    % Convertir le Type en atom si c'est une string
    ( atom(Type) -> TypeAtom = Type ; atom_string(TypeAtom, Type) ),
    format(user_error, 'DEBUG: TypeAtom=~w~n', [TypeAtom]),
    catch(
        call_ai(TypeAtom, Color, M),
        E,
        (
            format(user_error, 'ERROR in call_ai: ~w~n', [E]),
            fail
        )
    ),
    !,
    format(user_error, 'DEBUG: call_ai returned M=~w~n', [M]),
    ( var(M) ->
        format(user_error, 'WARNING: call_ai returned unbound variable~n', []),
        fail
    ; true ),
    ( valid_column(M) -> 
        Move = M,
        format(user_error, 'DEBUG: Move is valid: ~w~n', [Move])
    ; 
        format(user_error, 'WARNING: Invalid move ~w, choosing random~n', [M]),
        findall(C, valid_column(C), ValidCols),
        format(user_error, 'DEBUG: Valid columns: ~w~n', [ValidCols]),
        ( ValidCols = [] ->
            format(user_error, 'ERROR: No valid columns found!~n', []),
            fail
        ;
            random_member(Move, ValidCols),
            format(user_error, 'DEBUG: Random move chosen: ~w~n', [Move])
        )
    ).

% Les clauses utilisent maintenant des ATOMS
call_ai(aiRand, Color, Move) :- 
    format(user_error, 'DEBUG: Calling aiV1 for aiRand with Color=~w~n', [Color]),
    !,
    aiV1(Move, player(Color,_)),
    format(user_error, 'DEBUG: aiV1 returned Move=~w~n', [Move]).
    
call_ai(aiV2, Color, Move) :- 
    format(user_error, 'DEBUG: Calling aiV2 with Color=~w~n', [Color]),
    !,
    aiV2(Move, player(Color,_)),
    format(user_error, 'DEBUG: aiV2 returned Move=~w~n', [Move]).
    
call_ai(aiMinMax, Color, Move) :- 
    format(user_error, 'DEBUG: Calling ai (MinMax) with Color=~w~n', [Color]),
    !,
    ia_choisir_coup(Color, Move),
    format(user_error, 'DEBUG: ai returned Move=~w~n', [Move]).
    
call_ai(aiOp, Color, Move) :- 
    format(user_error, 'DEBUG: Calling aiOp with Color=~w~n', [Color]),
    !,
    ia_op_choisir_coup(Color, Move),
    format(user_error, 'DEBUG: aiOp returned Move=~w~n', [Move]).

call_ai(Type, Color, _) :-
    format(user_error, 'ERROR: Unknown AI type: ~w for color ~w~n', [Type, Color]),
    fail.

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
