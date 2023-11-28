(** Representation of the board state.
    This module represents the state of the board as it is being played,
    including the board's current state, the possible moves that can be made,
    and functions that cause the state to change. *)

type round
(** The abstract type of values representing the round of the game. *)

exception NoPiece
(** Raised when an unknown piece identifier is encountered. This occurs either
 when the current player chooses an invalid piece to move, when the
  tile that is selected is not occupied by a piece belonging to the current
  player. *)

exception WrongPlayer
(** Raised when the current player attempts to move the other player's piece. *)

exception InvalidJump
(** Raised when a jump isn't possible, that is there is no opponent piece to take
    and jump*)

val starting_round : Board.player -> Board.player -> round
(** Initializes the first round of the game *)

val make_round :
  Board.board -> Board.player -> Board.player -> Board.player -> round
(** [make_round b p1 p2 cp] is the round with board [b], player 1 [p1], player 2
    [p2], and current player [cp] *)

val get_board : round -> Board.board
(** [get_board round] returns the board for the current round of the game. *)

val get_current_player : round -> Board.player
(** [get_current_player round] returns the current player for the current round 
    of the game. *)

val set_current_player : round -> Board.player -> unit
(** [set_current_player round p] sets the current player for the round [round]
    of the game to be [p]. *)

val get_p1 : round -> Board.player
(** [get_p1 round] returns player 1 for the current round of the game. *)

val get_p2 : round -> Board.player
(** [get_p2 round] returns  player 2 for the current round of the game. *)

val is_empty_tile : round -> char -> int -> bool
(** [is_empty_tile round col row] is true iff a piece can move to the location 
    specified by [col] [row] during the round [round] otherwise false. Requires:
    [col] in 'A' to 'H' and [row] is in 1 to 8*)

val coord : char -> int -> string
(** [coord col row] is the string representation of a coordinate at column [col] 
    and row [row]*)

val list_simple_moves : round -> char -> int -> string list
(** [list_simple_moves round col row] is the list of coordinates reachable from 
    the simplest diagonial move to an empty space.

      - if the tile that is selected is not occupied by a piece belonging to the 
        current player, then  Raises [exception NoPiece]

      - if the current player attempts to move the other player's piece, raises 
        [exception WrongPlayer]

    Requires: [col] in 'A' to 'H' and [row] is in 1 to 8 *)

val list_jump_moves : round -> char -> int -> string list
(** [list_jump_moves round col row] is the list of coordinates reachable by 
jumping the opposing player's piece. *)

val iss_king : round -> char -> int -> bool
(** [iss_king round col row] returns true if for that round the tile at [col]
    [row] contains a piece with the is_king field eqaul to true, otherwise
   returns false *)

val update_king : round -> char -> int -> unit
(** [update_king round col row] updates the is_king field of the tile depending
    wether the player reached their other side of the board (this means row=8
    for player2 and row =1 for player 1) *)

val list_backwards_moves : round -> char -> int -> string list
(*** [list_backwards_move round col row] is the list of coordinates reachable
    from the simplest backwards diagonial move to an empty space. Backwards
    movements are only included for pieces that are kings.
      - if the tile that is selected is not occupied by a piece belonging to the
        current player, then  Raises [exception NoPiece]

      - if the current player attempts to move the other player's piece, raises
        [exception WrongPlayer]

    Requires: [col] in 'A' to 'H' and [row] is in 1 to 8 *)

val jump_backwards_moves : round -> char -> int -> string list
(** [jump_backwards_moves round col row] is the list of coordinates reachable by 
    backwards jumping the opposing player's piece. This is a king function *)

val all_backward_jumps : round -> char -> int -> string list
(** [all_backward_jumps round col row] returns a string list of all possible 
    simple diagonal moves a piece can do
    - if the piece is a king (iss_king =true) this function appends the 
        backwards jumps possible to the foward jumps possible
    - if piece is not a king the function simply returns the foward moves 
        possible represented by list_jump_moves *)

val add_backward_moves : round -> char -> int -> string list
(** [add_backwards_moves r col row] is the regular list of simple moves possible 
    if [is_king] is false for the tile at row [row] and column [col] during round
    [round]. If [is_king] is true then it is the list of possible backwards moves
    and possible forwards moves. *)

val move : round -> char -> int -> char -> int -> unit
(** [move round col row new_col new_row] moves a piece from [col row] to 
    [new_col new_row] during the round [round]. Requires: [col] and [new_col] in
    'A' to 'H' and [row] [new_row] is in 1 to 8. *)

val jump : round -> char -> int -> char -> int -> unit
(** [jump round col row new_col new_row] moves a piece from [col row] to 
    [new_col new_row] during the round [round] while jumping a piece. Requires: 
    [col] and [new_col] in 'A' to 'H' and [row] [new_row] is in 1 to 8. *)

val is_game_won : round -> bool
(** [is_game_won r] checks if the game is over during round [r] of the game. *)

val end_game : Board.player -> unit
(** [end_game p] ends the game and prints out the player [p] who won. *)
