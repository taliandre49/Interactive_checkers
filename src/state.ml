open Board

type round = {
  board : board;
  player1 : player;
  player2 : player;
  mutable current_player : player;
}

exception NoPiece
exception WrongPlayer
exception InvalidJump
exception NoPlayer

let starting_round p1 p2 =
  {
    board = starting_board p1 p2;
    player1 = p1;
    player2 = p2;
    current_player = p1;
  }

let make_round b p1 p2 cp =
  { board = b; player1 = p1; player2 = p2; current_player = cp }

let get_board round = round.board
let get_current_player round = round.current_player
let set_current_player round p = round.current_player <- p
let get_p1 round = round.player1
let get_p2 round = round.player2
let is_empty_tile round col row = is_empty (get_tile round.board col row)
let coord col row = String.make 1 col ^ string_of_int row

let score_calculator (opponent : player) : int =
  match opponent with
  | None -> raise NoPiece
  | Player p ->
      let score = 12 - p.num_pieces in
      score

(* [update_king] updates the is_king field of the tile depending wether the player reached the other side of the board *)
let update_king round col row =
  let piece = get_tile round.board col row in
  if (piece.color = Black && row = 8) || (piece.color = Black && row = 1) then
    piece.is_king <- true

let iss_king round col row = (get_tile round.board col row).is_king

let list_simple_moves round col row =
  let piece_tile = get_tile round.board col row in
  if is_empty piece_tile then raise NoPiece
  else if
    tile_rep piece_tile <> " " ^ get_initial round.current_player
    && tile_rep piece_tile <> "♕" ^ get_initial round.current_player
  then raise WrongPlayer
  else if round.current_player = round.player1 then
    let next_row = row - 1 in
    let next_col_left = letter_from_index (index col - 1) in
    let next_col_right = letter_from_index (index col + 1) in
    if
      next_row >= 1 && next_col_left >= 'A' && next_col_right <= 'H'
      && is_empty_tile round next_col_left next_row
      && is_empty_tile round next_col_right next_row
    then [ coord next_col_left next_row; coord next_col_right next_row ]
    else if
      next_row >= 1 && next_col_left >= 'A'
      && is_empty_tile round next_col_left next_row
    then [ coord next_col_left next_row ]
    else if
      next_row >= 1 && next_col_right <= 'H'
      && is_empty_tile round next_col_right next_row
    then [ coord next_col_right next_row ]
    else []
  else if round.current_player = round.player2 then
    let next_row = row + 1 in
    let next_col_left = letter_from_index (index col - 1) in
    let next_col_right = letter_from_index (index col + 1) in
    if
      next_row < 8 && next_col_left >= 'A' && next_col_right <= 'H'
      && is_empty_tile round next_col_left next_row
      && is_empty_tile round next_col_right next_row
    then [ coord next_col_left next_row; coord next_col_right next_row ]
    else if
      next_row < 8 && next_col_left >= 'A'
      && is_empty_tile round next_col_left next_row
    then [ coord next_col_left next_row ]
    else if
      next_row < 8 && next_col_right <= 'H'
      && is_empty_tile round next_col_right next_row
    then [ coord next_col_right next_row ]
    else []
  else raise WrongPlayer

let occupied_by_opponent round col row =
  if round.current_player = round.player1 then
    (get_tile round.board col row).occupant = round.player2
  else (get_tile round.board col row).occupant = round.player1

let list_jump_moves round col row =
  let piece_tile = get_tile round.board col row in
  if is_empty piece_tile then raise NoPiece (* copied code above *)
  else if
    tile_rep piece_tile <> " " ^ get_initial round.current_player
    && tile_rep piece_tile <> "♕" ^ get_initial round.current_player
  then raise WrongPlayer
  else if round.current_player = round.player1 then
    let next_row = row - 1 in
    let next_col_left = letter_from_index (index col - 1) in
    let next_col_right = letter_from_index (index col + 1) in
    let jump_row = row - 2 in
    let jump_col_left = letter_from_index (index col - 2) in
    let jump_col_right = letter_from_index (index col + 2) in
    if
      next_row >= 2 && next_col_left > 'A'
      && next_col_right < 'H' (* can jump right and left*)
      && occupied_by_opponent round next_col_left next_row
      && occupied_by_opponent round next_col_right next_row
      && is_empty_tile round jump_col_left jump_row
      && is_empty_tile round jump_col_right jump_row
    then [ coord jump_col_left jump_row; coord jump_col_right jump_row ]
    else if
      next_row >= 2
      && next_col_left > 'A' (* can only jump left *)
      && occupied_by_opponent round next_col_left next_row
      && is_empty_tile round jump_col_left jump_row
    then [ coord jump_col_left jump_row ]
    else if
      next_row >= 2
      && next_col_right < 'H' (* can only jump right*)
      && occupied_by_opponent round next_col_right next_row
      && is_empty_tile round jump_col_right jump_row
    then [ coord jump_col_right jump_row ]
    else []
  else if round.current_player = round.player2 then
    let next_row = row + 1 in
    let next_col_left = letter_from_index (index col - 1) in
    let next_col_right = letter_from_index (index col + 1) in
    let jump_row = row + 2 in
    let jump_col_left = letter_from_index (index col - 2) in
    let jump_col_right = letter_from_index (index col + 2) in
    if
      next_row <= 7 && next_col_left > 'A' && next_col_right < 'H'
      && occupied_by_opponent round next_col_left next_row
      && occupied_by_opponent round next_col_right next_row
      && is_empty_tile round jump_col_left jump_row
      && is_empty_tile round jump_col_right jump_row
    then [ coord jump_col_left jump_row; coord jump_col_right jump_row ]
    else if
      next_row <= 7 && next_col_left > 'A'
      && occupied_by_opponent round next_col_left next_row
      && is_empty_tile round jump_col_left jump_row
    then [ coord jump_col_left jump_row ]
    else if
      next_row <= 7 && next_col_right < 'H'
      && occupied_by_opponent round next_col_right next_row
      && is_empty_tile round jump_col_right jump_row
    then [ coord jump_col_right jump_row ]
    else []
  else raise WrongPlayer

(* [list_backwards_moves] returns a list of possible backwards moves for a given piece at [col] [row] *)
let list_backwards_moves round col row =
  let piece_tile = get_tile round.board col row in
  if is_empty piece_tile then raise NoPiece (* copied code above *)
  else if tile_rep piece_tile = " " ^ get_initial round.current_player then []
  else if tile_rep piece_tile <> "♕" ^ get_initial round.current_player then
    raise WrongPlayer
  else if round.current_player = round.player1 then
    let next_row_backward = row + 1 in
    let next_col_left = letter_from_index (index col - 1) in
    let next_col_right = letter_from_index (index col + 1) in
    if
      next_row_backward < 8 && next_col_left >= 'A'
      && next_col_right <= 'H' (* can move right and left backward*)
      && is_empty_tile round next_col_left next_row_backward
      && is_empty_tile round next_col_right next_row_backward
    then
      [
        coord next_col_left next_row_backward;
        coord next_col_right next_row_backward;
      ]
    else if
      next_row_backward < 8
      && next_col_left >= 'A' (* can move left backward*)
      && is_empty_tile round next_col_left next_row_backward
    then [ coord next_col_left next_row_backward ]
    else if
      next_row_backward < 8 && next_col_right <= 'H'
      && is_empty_tile round next_col_right next_row_backward
    then [ coord next_col_right next_row_backward ]
    else []
  else if round.current_player = round.player2 then
    let next_row_backward = row - 1 in
    let next_col_left = letter_from_index (index col - 1) in
    let next_col_right = letter_from_index (index col + 1) in
    if
      next_row_backward >= 1 && next_col_left >= 'A' && next_col_right <= 'H'
      && is_empty_tile round next_col_left next_row_backward
      && is_empty_tile round next_col_right next_row_backward
    then
      [
        coord next_col_left next_row_backward;
        coord next_col_right next_row_backward;
      ]
    else if
      next_row_backward >= 1 && next_col_left >= 'A'
      && is_empty_tile round next_col_left next_row_backward
    then [ coord next_col_left next_row_backward ]
    else if
      next_row_backward >= 1 && next_col_right <= 'H'
      && is_empty_tile round next_col_right next_row_backward
    then [ coord next_col_right next_row_backward ]
    else []
  else raise WrongPlayer

let jump_backwards_moves round col row =
  let piece_tile = get_tile round.board col row in
  if is_empty piece_tile then raise NoPiece (* copied code above *)
  else if tile_rep piece_tile = " " ^ get_initial round.current_player then []
  else if tile_rep piece_tile <> "♕" ^ get_initial round.current_player then
    raise WrongPlayer
  else if round.current_player = round.player1 then
    let next_row = row + 1 in
    let next_col_left = letter_from_index (index col - 1) in
    let next_col_right = letter_from_index (index col + 1) in
    let jump_row = row + 2 in
    let jump_col_left = letter_from_index (index col - 2) in
    let jump_col_right = letter_from_index (index col + 2) in
    if
      next_row <= 7 && next_col_left > 'A'
      && next_col_right < 'H' (* can jump right and left*)
      && occupied_by_opponent round next_col_left next_row
      && occupied_by_opponent round next_col_right next_row
      && is_empty_tile round jump_col_left jump_row
      && is_empty_tile round jump_col_right jump_row
    then [ coord jump_col_left jump_row; coord jump_col_right jump_row ]
    else if
      next_row <= 7
      && next_col_left > 'A' (* can only jump left *)
      && occupied_by_opponent round next_col_left next_row
      && is_empty_tile round jump_col_left jump_row
    then [ coord jump_col_left jump_row ]
    else if
      next_row <= 7
      && next_col_right < 'H' (* can only jump right*)
      && occupied_by_opponent round next_col_right next_row
      && is_empty_tile round jump_col_right jump_row
    then [ coord jump_col_right jump_row ]
    else []
  else if round.current_player = round.player2 then
    let next_row = row - 1 in
    let next_col_left = letter_from_index (index col - 1) in
    let next_col_right = letter_from_index (index col + 1) in
    let jump_row = row - 2 in
    let jump_col_left = letter_from_index (index col - 2) in
    let jump_col_right = letter_from_index (index col + 2) in
    if
      next_row >= 2 && next_col_left > 'A' && next_col_right < 'H'
      && occupied_by_opponent round next_col_left next_row
      && occupied_by_opponent round next_col_right next_row
      && is_empty_tile round jump_col_left jump_row
      && is_empty_tile round jump_col_right jump_row
    then [ coord jump_col_left jump_row; coord jump_col_right jump_row ]
    else if
      next_row >= 2 && next_col_left > 'A'
      && occupied_by_opponent round next_col_left next_row
      && is_empty_tile round jump_col_left jump_row
    then [ coord jump_col_left jump_row ]
    else if
      next_row >= 2 && next_col_right < 'H'
      && occupied_by_opponent round next_col_right next_row
      && is_empty_tile round jump_col_right jump_row
    then [ coord jump_col_right jump_row ]
    else []
  else raise WrongPlayer

let all_backward_jumps round col row =
  if iss_king round col row then
    let all_jumps_list =
      list_jump_moves round col row @ jump_backwards_moves round col row
    in
    all_jumps_list
  else list_jump_moves round col row

(* [add_backwards_moves returns the regular list of simple moves possible if is_king is false
    and if is_king is true, [add_backward_moves] returns the list of possible backwards
    moves appended to regualr simple moves list] *)
let add_backward_moves round col row =
  if iss_king round col row then
    let allmoves_list =
      list_simple_moves round col row @ list_backwards_moves round col row
    in
    allmoves_list
  else list_simple_moves round col row

let move round col row new_col new_row =
  (* the tile to move to *)
  let next_tile = get_tile round.board new_col new_row in
  (* the tile to move to edited to hold player *)
  let new_tile =
    occupy_tile round.current_player next_tile (iss_king round col row)
  in
  (get_row round.board new_row).(index new_col) <- new_tile;
  update_king round new_col new_row;

  (* the initial tile *)
  let start_tile = get_tile round.board col row in
  (* the initial tile now empty*)
  let empty_tile = occupy_tile None start_tile false in
  (get_row round.board row).(index col) <- empty_tile

let jump round col row new_col new_row =
  (* the tile to move to *)
  let next_tile = get_tile round.board new_col new_row in
  (* the tile to move to edited to hold player *)
  let new_tile =
    occupy_tile round.current_player next_tile (iss_king round col row)
  in
  (get_row round.board new_row).(index new_col) <- new_tile;
  update_king round new_col new_row;

  (* the initial tile *)
  let start_tile = get_tile round.board col row in
  (* the initial tile now empty*)
  let empty_tile = occupy_tile None start_tile false in
  (get_row round.board row).(index col) <- empty_tile;
  let jumped_col = letter_from_index ((index new_col + index col) / 2) in
  let jumped_row = (new_row + row) / 2 in
  let jumped_tile = get_tile round.board jumped_col jumped_row in
  let taken_player = jumped_tile.occupant in
  match taken_player with
  | Player p -> (
      p.num_pieces <- p.num_pieces - 1;
      (get_row round.board jumped_row).(index jumped_col) <-
        { jumped_tile with occupant = None };
      match round.current_player with
      | None -> raise NoPiece
      | Player self -> self.score <- score_calculator taken_player)
  | None -> raise InvalidJump

let is_game_won (round : round) =
  let cp = round.current_player in
  cp |> get_score |> int_of_string = 12

let end_game (winner : player) =
  match winner with
  | None -> raise NoPlayer
  | Player p ->
      print_endline
        ("Congratulations! Player " ^ String.make 1 p.initial ^ " has won!")
