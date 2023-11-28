(** Representation of the board.
    This module represents the checkers board and its structure, including the 
    players and ways to access the board. *)

type player =
  | None
  | Player of { initial : char; mutable num_pieces : int; mutable score : int }

(** The type of values representing the players of the game.
    A player is either [None] or is represented by their initial, piece count, 
    and score. *)

type tile_color = Black | White

(** The type that represents the colour of a tile. Pieces only play on Black 
    tiles. *)

type tile = { color : tile_color; occupant : player; mutable is_king : bool }
(** The type that represents a tile on the checker board. *)

type row = tile array
(** The type that represents one row (characterized by numbers 1-8) of a checker
    board. *)

type board = row list
(** The type that represents the checkers board. *)

type game = { board : board; player1 : player; player2 : player }
(** The type that represents a game between [player1] and [player2] with board
    [board]. *)

val occupy_tile : player -> tile -> bool -> tile
(** [occupy_tile t p by_king] gives the tile of [t]'s colour that is occupied by
    player [p] and is a king when [by_king = true]. *)

val empty_black : tile
(** [empty_black] is the empty Black tile with no piece on it. *)

val empty_white : tile
(** [empty_white] is the empty White tile with no piece on it. *)

val is_empty : tile -> bool
(** [is_empty t] is true when [t] is occupied by [None] and false otherwise. *)

val empty_black_arr : tile array
(** [empty_black_arr] is the row of empty Black tiles. *)

val empty_white_arr : tile array
(** [empty_white_arr] is the row of empty White tiles. *)

val even_row : tile array -> tile array
(** [even_row t] creates an even row based on empty row [t]. An empty row has an
    even number (2, 4, 6, 8) for board row position and starts with a white tile. 
    Requires: [t] is a row of empty White tiles. *)

val odd_row : tile array -> tile array
(** [odd_row t] creates an odd row based on empty row [t]. An empty row has an
    odd number (1, 3, 5, 7) for board row position and starts with a black tile. 
    Requires: [t] is a row of empty Black tiles. *)

val empty_board : tile array list
(** [empty_board] is the board with no pieces on any tiles. *)

val fill_row : player -> row -> tile array
(** [fill_row p r] returns the row with Black tiles occupied by player [p]'s
    pieces. *)

val starting_board : player -> player -> board
(** [starting_board] is the initial board with player 2 filling the Black tiles 
    in the first 3 rows and player 1 in the bottom 3 rows. *)

val get_row : board -> int -> row
(** [get_row b row_number] is the row at coordinate location [row_number].
    Requires: [row_number] is an int from 1..8. *)

val index : char -> int
(** [index col_letter] is the index in the board rows of [col_letter].
    Requires: [col_letter] in 'A'..'H'. *)

val letter_from_index : int -> char
(** [letter_from_index i] is the column letter with index [i] in the board rows.
  Requires: [i] is in 0..7. *)

val get_tile : board -> char -> int -> tile
(** [get_tile b col_letter row_number] is the tile at coordinate location [col_letter][row_number].
    Requires: [col_letter] in 'A'..'H' and [row_number] is in 1..8. *)

val get_initial : player -> string
(** [get_initial player] is the inital of the player [player].
    Requires: [player] is a valid player *)

val get_num_pieces : player -> int
(** [get_num_pieces p] is the number of pieces owned by player [p]. *)

val tile_rep : tile -> string
(** [tile_rep tile] gives the interface representation of the tile. 
    If the tile is occupied, it will give the player's initial.
    It will also indicate whether a player's piece on the tile is a king. *)

val get_score : player -> string
(** [get_score player] is the current score of the player [player].
    Requires: [player] is a valid player *)

val print_board : board -> unit
(** [print_board b] prints a representation of board [b]. *)

val create_player : char -> player
(** [create_player i] creates a player with initial [i]. *)

val init_game : board -> player -> player -> game
(** [init_game b p1 p2] creates a game with board [b] and players [p1] and [p2]. *)

val start_game : char -> char -> game
(** [init_game b i1 i2] creates a game with board [b] and players with initials 
    [i1] and [i2]. *)