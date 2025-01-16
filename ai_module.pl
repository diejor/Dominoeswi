:- module(ai_module, [
    bestMove/3,
    clear_all/0,
    known_action/3,
    player_hand/1
]).

:- use_module(library(debug)).

/* -------------------------------------------------------------------------
   1. Declare dynamic predicates so that we can assert/retract them at runtime
   ------------------------------------------------------------------------- */
:- dynamic known_action/3.
:- dynamic player_hand/1.

/* -------------------------------------------------------------------------
   2. clear_all/0
      This will be called before we rebuild the knowledge base to prevent
      stale facts from persisting.
   ------------------------------------------------------------------------- */
clear_all :-
    retractall(known_action(_, _, _)),
    retractall(player_hand(_)).

/* -------------------------------------------------------------------------
   3. bestMove/3
      This is our AI logic. The signature is bestMove(ID, L, R):
        - ID: an identifier (can be anything, e.g., "piece0" or an index)
        - L, R: the piece's left/right values

      In a naive approach, we:
         - Look at the known actions to see which sides are open
         - Check the player's hand to see if there's a piece that can match
           the open side
         - Return the first piece that fits
         - If none is found, fail (leading to "No valid moves found")
   ------------------------------------------------------------------------- */
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

/* -------------------------------------------------------------------------
   4. Helper to find which numbers are "open" in the chain.
      This is optional/illustrative. For a real domino logic, you'd determine
      the leftmost and rightmost numbers that are still open on the board.

      For example, if the known actions say:
         - player1 placed piece(6,6) on "left"
         - player2 placed piece(6,5) on "right"
      then presumably "6" is no longer open on the left side, but "5" might
      be open on the right, and so on.

      For a minimal example, let's just gather all left/right from known actions
      that are said to be on "left" or "right" and treat them as "open" numbers.
   ------------------------------------------------------------------------- */
get_open_numbers(Actions, OpenNumbers) :-
    findall(X, (
        member((_, piece(L,R), Side), Actions),
        side_open_number(Side, L, R, X)
    ), Nums),
    list_to_set(Nums, OpenNumbers).  % remove duplicates

/* -------------------------------------------------------------------------
   side_open_number/4
   - A small helper that picks which side is "open."
   - For instance, if side == "left", maybe R is the open number, etc.
   - This is just a placeholder and can be adjusted to reflect actual rules.
   ------------------------------------------------------------------------- */
side_open_number("left",  L, R, X) :- X is L + R.  % purely illustrative
side_open_number("right", L, R, X) :- X = L.      % purely illustrative
side_open_number(_,       L, _,  L).             % fallback

/* -------------------------------------------------------------------------
   can_play_piece/3
   - True if the piece's left or right matches one of the open numbers.
   ------------------------------------------------------------------------- */
can_play_piece(HL, HR, OpenNumbers) :-
    (   member(HL, OpenNumbers)
    ;   member(HR, OpenNumbers)
    ).

