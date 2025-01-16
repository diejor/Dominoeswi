:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(debug)).

:- use_module(ai_module).

:- debug(http).           
:- debug(ai_module).      
:- initialization(run_server).

run_server :-
    debug(http, 'Starting HTTP server on port 5000...', []),
    http_server(http_dispatch, [port(5000)]),
    debug(http, 'Server started successfully.', []).

:- http_handler(root(update_facts), handle_update_facts, [method(post)]).

handle_update_facts(Request) :-
    debug(http, 'Received request for /update_facts', []),

    catch(http_read_json_dict(Request, DictIn), Error, true),
    (   var(Error)
    ->  debug(http, 'Received JSON payload: ~w', [DictIn]),
        rebuild_knowledge_base(DictIn),
        execute_query
    ;   debug(http, 'Error reading JSON payload: ~w', [Error]),
        reply_json_dict(
            _{ error: 'Invalid JSON payload', details: Error },
            [ status(400) ]
        )
    ).

rebuild_knowledge_base(Dict) :-
    clear_all,

    (   _{knownActions: Actions} :< Dict ->
        forall(member(Action, Actions), assert_known_action(Action))
    ;   debug(http, 'No known actions in JSON payload.', [])
    ),

    (   _{myHand: Hand} :< Dict ->
        forall(member(Piece, Hand), assert_player_hand(Piece))
    ;   debug(http, 'No player hand in JSON payload.', [])
    ),

    debug(http, 'Knowledge base successfully rebuilt.', []).

assert_known_action(Action) :-
    Action = _{player: Player, piece: Piece, side: Side},
    Piece = _{left: Left, right: Right},
    assertz(known_action(Player, piece(Left, Right), Side)).

assert_player_hand(Piece) :-
    Piece = _{left: Left, right: Right},
    assertz(player_hand(piece(Left, Right))).

execute_query :-
    findall(Piece, player_hand(Piece), Hands),
    debug(ai_module, 'Player hand: ~w', [Hands]),
    findall((Player, Piece, Side), known_action(Player, Piece, Side), Actions),
    debug(ai_module, 'Known actions: ~w', [Actions]),

    % Run the query
    (   catch(ai_module:bestMove(ID, L, R), QueryError, true)
    ->  (   var(QueryError)
        ->  debug(ai_module, 's(CASP) Query executed: ID=~w, Left=~w, Right=~w', [ID, L, R]),
            % 200 OK with move
            reply_json_dict(_{chosenId: ID, left: L, right: R}, [status(200)])
        ;   debug(ai_module, 'Error during query execution: ~w', [QueryError]),
            reply_json_dict(
                _{ error: 'Query execution failed', details: QueryError },
                [status(500)]
            )
        )

    ;   
        debug(ai_module, 'No valid moves found => Pass', []),
        reply_json_dict(_{chosenId: 'pass'}, [status(200)])
    ).


