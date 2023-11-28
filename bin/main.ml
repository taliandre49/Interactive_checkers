open Game
open Board
open State

(** [instructions p1 p2] prints the instructions for the checkers game with 
    players [p1] and [p2]. *)
let instructions p1 p2 =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\nInstructions.\n";
  print_endline
    "1. During each turn, the board will print and a player can move their \
     piece.";
  print_endline
    "2. Each piece can move forward diagonally to an empty space. \n\
    \   Kings can move backwards to an empty diagonal space.";
  print_endline
    "3. To become a king, a piece must reach the opponent's side of the board. \
     A king piece is signified by a ♕";
  print_endline
    "4. A piece can also jump an opponent's piece to capture it by jumping \n\
    \   diagonally over their piece.";
  print_endline
    "\nThe game ends when all pieces have been captured by the other player.\n";
  print_endline ("Player 1's pieces are repesented with " ^ get_initial p1);
  print_endline
    ("Player 2's pieces are repesented with " ^ get_initial p2 ^ " \n");
  print_endline
    "The alternating spaces on the checkerboard are represented by ░ tiles and \
     empty tiles.";
  print_endline
    "Player 1 will start the game. \n\n\
     To quit the game, simply enter \"QUIT\" when prompted for a coordinate. \n"

(** [move_list_of_string lst] prints the list [lst]. *)
let rec move_list_of_string lst =
  match lst with
  | [] -> ""
  | [ h ] -> h
  | h :: t -> h ^ ", " ^ move_list_of_string t

(** [int_of_ascii_int row] is the row number that corresponds to the ascii
    representation of the row int. *)
let int_of_ascii_int row = Char.code row - 48

(** [valid_coord coord] is [true] if the string represents a valid board coordinate 
    and [false] otherwise. *)
let valid_coord coord =
  if String.length coord <> 2 then false
  else if Char.code coord.[0] < 65 || Char.code coord.[0] > 72 then false
  else if int_of_ascii_int coord.[1] < 1 || int_of_ascii_int coord.[1] > 8 then
    false
  else true

let rec enter_valid prev =
  if not (valid_coord prev) then
    print_endline "\nPlease enter a valid coordinate.";
  print_endline "Piece's Coordinate: [A1 ... H8]:";
  print_string "> ";
  match read_line () |> String.uppercase_ascii with
  | coord ->
      if coord = "QUIT" then exit 0
      else if valid_coord coord then coord
      else enter_valid coord

(** [make_move_list round coord] is the tiles that the piece at [coord]
    can move to during round [round] and the possible jumps in a tuple of the form 
    (simple moves, jumps). *)
let rec make_move_list round coord =
  let curr_col = !coord.[0] in
  let curr_row_letter = int_of_ascii_int !coord.[1] in
  match
    ( add_backward_moves round curr_col curr_row_letter,
      all_backward_jumps round curr_col curr_row_letter )
  with
  | [], [] ->
      print_endline "No possible moves. Pick another piece.\n";
      coord := enter_valid "A1";
      make_move_list round coord
  | exception WrongPlayer ->
      (* Both will have same exception *)
      print_endline "That is not your piece. Pick another piece.\n";
      coord := enter_valid "A1";
      make_move_list round coord
  | exception NoPiece ->
      print_endline "There is no piece there. Pick another space.\n";
      coord := enter_valid "A1";
      make_move_list round coord
  | move_list, jump_list -> (move_list, jump_list)

let can_jump round coord =
  let curr_col = !coord.[0] in
  let curr_row_letter = int_of_ascii_int !coord.[1] in
  match list_jump_moves round curr_col curr_row_letter with
  | [] | (exception WrongPlayer) | (exception NoPiece) -> false
  | _ -> true

(** [move_piece move_list jump_list coord new_coord round] moves a piece from [coord] to 
    [new_coord] during round [round] iff the new coordinate is in [move_list] or 
    jumps if new_coord is in [jump_list]*)
let rec move_piece move_list jump_list (coord : string) (new_coord : string)
    round =
  if
    List.mem new_coord move_list = false && List.mem new_coord jump_list = false
  then move_piece move_list jump_list coord (enter_valid "") round
  else if List.mem new_coord move_list then (
    move round coord.[0]
      (int_of_ascii_int coord.[1])
      new_coord.[0]
      (int_of_ascii_int new_coord.[1]);
    false)
  else (
    jump round coord.[0]
      (int_of_ascii_int coord.[1])
      new_coord.[0]
      (int_of_ascii_int new_coord.[1]);
    true)

let rec multiple_jumps round coord =
  if can_jump round coord then (
    let move_tuple = make_move_list round coord in
    let jump_list = snd move_tuple in
    let current_p = get_current_player round in
    let text_color =
      if current_p = get_p1 round then ANSITerminal.blue else ANSITerminal.red
    in
    print_board (get_board round);
    print_endline "You have the ability to make another jump.";
    ANSITerminal.print_string [ text_color ]
      ("Possible jumps: " ^ move_list_of_string jump_list ^ "\n");
    ANSITerminal.print_string [ text_color ]
      "\nWhere would you like to move your piece?";
    let new_coord = ref (enter_valid "") in
    let _ = move_piece [] jump_list !coord !new_coord round in
    if !new_coord.[1] <> '1' || !new_coord.[1] <> '8' then
      if can_jump round new_coord then multiple_jumps round new_coord)
  else ()

(** [play_round round] is the user prompts for the given round [round] *)
let play_round round =
  let current_p = get_current_player round in
  let text_color =
    if current_p = get_p1 round then ANSITerminal.blue else ANSITerminal.red
  in
  ANSITerminal.print_string [ text_color ]
    ("Current player: " ^ get_initial current_p ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("Current score "
    ^ get_initial (get_p1 round)
    ^ ": "
    ^ get_score (get_p1 round)
    ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("Current score "
    ^ get_initial (get_p2 round)
    ^ ": "
    ^ get_score (get_p2 round)
    ^ "\n");
  ANSITerminal.print_string [ text_color ]
    "What is the coordinate of the piece you would like to move?";
  let coordinate = ref (enter_valid "") in
  let move_tuple = make_move_list round coordinate in
  let move_list = fst move_tuple in
  let jump_list = snd move_tuple in
  ANSITerminal.print_string [ text_color ]
    ("\nPossible moves: " ^ move_list_of_string move_list ^ "\n");
  ANSITerminal.print_string [ text_color ]
    ("Possible jumps: " ^ move_list_of_string jump_list ^ "\n");
  ANSITerminal.print_string [ text_color ]
    "\nWhere would you like to move your piece?";
  let new_coord = ref (enter_valid "") in
  let did_a_jump =
    move_piece move_list jump_list !coordinate !new_coord round
  in
  let _ = if did_a_jump then multiple_jumps round new_coord in
  if get_current_player round = get_p1 round then
    set_current_player round (get_p2 round)
  else if get_current_player round = get_p2 round then
    set_current_player round (get_p1 round)
  else set_current_player round (get_current_player round);
  ANSITerminal.erase Screen;
  round

(** [play_game p1 p2 round] is the checkers game with players [p1] and [p2]
  during game round [round]. *)
let rec play_game p1 p2 round =
  if is_game_won round then round |> get_current_player |> end_game
  else
    let next_round = play_round round in
    print_board (get_board next_round);
    play_game p1 p2 next_round

(** [create_player known] creates a player unless its initial is the same as an 
    existing player's initial [known]. If the initial is not a capital letter then it 
    reprompts the user.*)
let rec create_player known =
  print_endline
    "The initial must be different from any existing player's initial. It must \
     also be a capital letter from A to Z.";
  print_string "> ";
  match read_line () with
  | initial ->
      if initial = known then create_player known
      else
        let ascii_char = Char.code (String.get initial 0) in
        if ascii_char > 64 && ascii_char < 91 then
          Board.create_player (Char.chr ascii_char)
        else create_player known

(* let file = "game_log.txt"
   let  write_file (current_status : string) =
     let oc = open_out file in
     Printf.fprintf oc "%s\n" current_status;
     close_out oc; *)

(** [main ()] prompts for the names of the two players, then starts the game. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nWelcome to the Boolin Boolean's Checkers game engine.\n";
  print_endline "Please enter the first player's initial.";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | initial ->
      let p1 =
        if int_of_char initial.[0] > 64 && int_of_char initial.[0] < 91 then
          Board.create_player (String.get (String.capitalize_ascii initial) 0)
        else create_player ""
      in
      print_endline "\nPlease enter the second player's initial.";
      let p2 = create_player (get_initial p1) in
      instructions p1 p2;
      print_endline "Here is the starting board.\n";
      print_board (starting_board p1 p2);
      let round = starting_round p1 p2 in
      play_game p1 p2 round

(* Execute the game engine. *)
let () = main ()
