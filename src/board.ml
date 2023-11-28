(** A player that is either None or holds a char represented their pieces *)
type player =
  | None
  | Player of { initial : char; mutable num_pieces : int; mutable score : int }

(* Only play on Black tiles *)
type tile_color = Black | White

type tile = { color : tile_color; occupant : player; mutable is_king : bool }
(** Represents one tile on checker board *)

type row = tile array
(** Represents one row (characterized by numbers 1-8) of a Checker board *)

type board = row list
(** Represents the Checkers board*)

type game = { board : board; player1 : player; player2 : player }

let get_color tile = tile.color

(** [occupy_tile t p is_king] gives the tile that is now occupied by player [p] 
    by a piece that is a king when [by_king = true]. *)
let occupy_tile (p : player) (t : tile) (by_king : bool) =
  { color = get_color t; occupant = p; is_king = by_king }

(** The empty Black tile *)
let empty_black = { color = Black; occupant = None; is_king = false }

(** The empty White tile *)
let empty_white = { color = White; occupant = None; is_king = false }

(** [is_empty t] returns true when [t] is occupied by None *)
let is_empty (t : tile) = t.occupant = None

let empty_white_arr = Array.make 8 empty_white
let empty_black_arr = Array.make 8 empty_black

(** An empty row at an even number (2, 4, 6, 8), starts with a white tile*)
let even_row arr =
  arr.(1) <- empty_black;
  arr.(3) <- empty_black;
  arr.(5) <- empty_black;
  arr.(7) <- empty_black;
  arr

(** An empty row at an odd number (1, 3, 5, 7), starts with a black tile*)
let odd_row arr =
  arr.(1) <- empty_white;
  arr.(3) <- empty_white;
  arr.(5) <- empty_white;
  arr.(7) <- empty_white;
  arr

(** The board with no pieces *)
let empty_board =
  [
    even_row empty_white_arr;
    odd_row empty_black_arr;
    even_row empty_white_arr;
    odd_row empty_black_arr;
    even_row empty_white_arr;
    odd_row empty_black_arr;
    even_row empty_white_arr;
    odd_row empty_black_arr;
  ]

(** [fill_row p r] returns the row with Black tiles filled in with player [p] *)
let fill_row (p : player) (r : row) =
  Array.map
    (fun x ->
      if is_empty x && x.color = Black then occupy_tile p x false else x)
    r

(** The starting board with player 2 filling the Black tiles in the first 3 rows 
    and player 1 in the bottom 3 rows *)
let starting_board (p1 : player) (p2 : player) : board =
  [
    even_row empty_white_arr |> fill_row p2;
    odd_row empty_black_arr |> fill_row p2;
    even_row empty_white_arr |> fill_row p2;
    odd_row empty_black_arr |> fill_row None;
    even_row empty_white_arr |> fill_row None;
    odd_row empty_black_arr |> fill_row p1;
    even_row empty_white_arr |> fill_row p1;
    odd_row empty_black_arr |> fill_row p1;
  ]

(** [get_row b row_number] is the row at coordinate location [row_number].
Requires: [row_number] is in 1..8 *)
let get_row (b : board) (row_number : int) = List.nth b (row_number - 1)

(** [index col_letter] is the index in the board rows of [col_letter].
Requires: [col_letter] in 'A'..'H'*)
let index col_letter = int_of_char col_letter - 65

(** [letter_from_index i] is the column letter with index [i] in the board rows.
Requires: [i] is in 0..7*)
let letter_from_index i = Char.chr (i + 65)

(** [get_tile b col_letter row_number] is the tile at coordinate location [col_letter][row_number].
Requires: [col_letter] in 'A'..'H' and [row_number] is in 1..8 *)
let get_tile (b : board) (col_letter : char) (row_number : int) =
  (get_row b row_number).(index col_letter)

(** [get_initial player] is the inital of the player [player].
    Requires: [player] is a valid player *)
let get_initial player =
  match player with
  | Player { initial; _ } -> String.make 1 initial
  | None -> failwith "None has no initial"

let get_num_pieces player =
  match player with
  | Player { num_pieces; _ } -> num_pieces
  | None -> failwith "None has no pieces"

(** [tile_rep tile] gives the interface representation of the tile. 
If the tile is occupied, it will give the player's initial. *)
let tile_rep (tile : tile) : string =
  if is_empty tile then if tile.color = Black then "  " else "░░"
  else if tile.is_king then "♕" ^ get_initial tile.occupant
  else " " ^ get_initial tile.occupant

(** [get_score player] is the current score of the player [player].
    Requires: [player] is a valid player *)
let get_score player : string =
  match player with
  | Player p -> string_of_int p.score
  | None -> failwith "None has no initial"

(** [print_board b] prints a representation of board [b]. *)
let print_board (board : board) : unit =
  print_endline "        A      B      C      D      E      F      G     H";
  print_endline "    ╔══════╤══════╤══════╤══════╤══════╤══════╤══════╤══════╗";

  for r = 1 to 8 do
    for c = 0 to 7 do
      if c = 0 then (
        print_string ("  " ^ string_of_int r ^ " " ^ "║");
        print_string
          ("  " ^ tile_rep (get_tile board (letter_from_index c) r) ^ "  │"))
      else if c = 7 then (
        print_endline
          ("  " ^ tile_rep (get_tile board (letter_from_index c) r) ^ "  ║");
        if r < 8 then
          print_endline
            "    ╟──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────╢"
        else if r = 8 then (
          print_endline
            "    ╚══════╧══════╧══════╧══════╧══════╧══════╧══════╧══════╝";
          print_string
            "       A      B      C      D       E      F      G      H"))
      else
        print_string
          ("  " ^ tile_rep (get_tile board (letter_from_index c) r) ^ "  │")
    done
  done;
  print_endline ""

(*let stringify_board (board : board ) : string list =
  let b : string list  = ["        A      B      C      D      E      F      G     H";
  "    ╔══════╤══════╤══════╤══════╤══════╤══════╤══════╤══════╗"] in
  let s : string  = "a";

  for r = 1 to 8 do
    for c = 0 to 7 do
      if c = 0 then (
        print_string ("  " ^ string_of_int r ^ " " ^ "║");
      print_string
        ("  " ^ tile_rep (get_tile board (letter_from_index c) r) ^ "  │"))
        s = "  " ^ string_of_int r ^ " " ^ "║" ^ "  " ^ (tile_rep (get_tile board (letter_from_index c) r) ^ "  │")
      else if c = 7 then (
        s = s ^ "  " ^ tile_rep (get_tile board (letter_from_index c) r) ^ "  ║" ^ " %s\n" ;
        b @ s;
        if r < 8 then

          b @ ("    ╟──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────╢ " ^ "%s\n")

        else if r = 8 then (
            b @ ("    ╚══════╧══════╧══════╧══════╧══════╧══════╧══════╧══════╝"
            ^ "%s\n" ^ "       A      B      C      D       E      F      G      H")))
      else
          s = s ^ ("  " ^ tile_rep (get_tile board (letter_from_index c) r) ^ "  │")
      done)
    done; *)

let create_player (initial : char) : player =
  Player { initial; num_pieces = 12; score = 0 }

let init_game (board : board) (player1 : player) (player2 : player) =
  { board; player1; player2 }

let start_game (p1_initial : char) (p2_initial : char) : game =
  let p1 = create_player p1_initial in
  let p2 = create_player p2_initial in
  { board = starting_board p1 p2; player1 = p1; player2 = p2 }