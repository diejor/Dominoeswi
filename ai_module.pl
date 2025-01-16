:- module(ai_module, [
    bestMove/3,
    clear_all/0,
    known_action/3,
    player_hand/1
]).

:- use_module(library(debug)).

:- dynamic known_action/3.
:- dynamic player_hand/1.

clear_all :-
    retractall(known_action(_, _, _)),
    retractall(player_hand(_)).

bestMove(ID, L, R) :-
    % First, gather all the current known actions and player's hand for reference:
    findall((Player, piece(Left,Right), Side), known_action(Player, piece(Left,Right), Side), Actions),
    findall(piece(HL,HR), player_hand(piece(HL,HR)), MyHand),

    debug(ai_module, 'bestMove/3 - known actions: ~w', [Actions]),
    debug(ai_module, 'bestMove/3 - player hand: ~w', [MyHand]),

    % We can define a helper to figure out which sides (numbers) are open:
    get_open_numbers(Actions, OpenNumbers),

    % Then attempt to find a piece that matches any open number:
    member(piece(HL, HR), MyHand),
    can_play_piece(HL, HR, OpenNumbers),
    % If we found a valid piece, unify:
    L = HL, R = HR,
    %
    % For ID, you can generate or keep it simple:
    format(string(ID), "piece(~w-~w)", [HL, HR]).

get_open_numbers(Actions, OpenNumbers) :-
    findall(X, (
        member((_, piece(L,R), Side), Actions),
        side_open_number(Side, L, R, X)
    ), Nums),
    list_to_set(Nums, OpenNumbers).  % remove duplicates

side_open_number("left",  L, R, X) :- X is L + R.  % purely illustrative
side_open_number("right", L, R, X) :- X = L.      % purely illustrative
side_open_number(_,       L, _,  L).             % fallback

can_play_piece(HL, HR, OpenNumbers) :-
    (   member(HL, OpenNumbers)
    ;   member(HR, OpenNumbers)
    ).

