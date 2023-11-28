(* Test Plan: Because of the nature of our game, some features (like the user
   interface, user input, coordinate checking in-game, writing valid moves, etc.
   could only be tested using play testing and manually playing through the game
   and trying various scenarios. We relied on manual testing for everything in the
   bin/main file and also for ending the game (whose functionality was in src/state)
   because those parts and their effects were dependent on what the user input for
   a prompt and could not be tested with only OUnit testing. Thus we used OUnit
   testing for all parts of src/board and all other parts of src/state.

   We still wanted to test the features of the board and movements correctly in with
   OUnit testing so we tested against the board itself for these tests. To test
   movements, we would check properties of the board before and after a move to make
   sure that it correctly reflects the movement that we made. That means a lot of
   our tests would make sure that trying to access a tile would raise the necessary
   exceptions or return the correct values. For example, we would check that a tile
   was empty before we moved a piece there and then we would check that that tile
   had a piece there that was owned by the correct player after the move. This
   implictly helped with our manual testing because we used the raising of these
   exceptions as a way to correctly give the right user output at any point. We
   started by testing the conditions of the starting board to make sure that the
   tiles were generated correctly. We then moved incrementally through moves one at
   a time, making sure that the move itself was valid, and checking that moving
   pieces correctly updated the board. We then tested more complex features such as
   jumping. While doing so, we verified that the score would increment properly.
   Each movement would work the same way we intended movement to work during a
   round of the game- first by moving a piece and then by changing the current
   player. This way we could make sure that our round updating should work during
   the user-interface version of the game. We would tile properties that should
   have changed due to a move during tests in addition to testing normal expected
   behaviour (such as unmoved pieces being able/unable to move).

   All tests were developed with glass box testing because we primarily needed to
   verify that the functions worked as we intended them to based on how we wrote
   them. This was especially important when we needed to debug functionality that
   appeared to work during the user-input part of the game but failed during certain
   test cases. We had some issues with the mutability of our board, for example,
   until we were able to fix it with thorough testing. Together, these pieces show
   the correctness of our system because the OUnit testing verifies the underlying
   code and functions for movement, the board structure, etc. that we directly use
   for the terminal-based part of the game that was manually tested. Because we
   wrote the user interface part as if each function returned/did exactly what we
   intended, this step of OUnit testing that output was critical to testing the base
   functionality of our game. Things like checking if the user was reprompted if they
   entered something in the wrong format was possible with manual testing but we
   could not check correctness of the underlying parts without using both approaches
   together which is why we tested in this way. In fact, using both let us easily
   detect errors or edge cases that would have been cumbersome and difficult to
   check with manual testing because it would have required us to set up the
   movements over and over again. Plus, some feature working in the manual test did
   not actually guarantee that the underlying function was doing the right thing so
   we needed to use both. *)

open OUnit2
open Game
open Board

let print_tile (t : tile) =
  match (t.color, t.occupant) with
  | Black, None -> "{Black, None}"
  | White, None -> "{White, None}"
  | Black, Player { initial; _ } ->
      "{Black, Player " ^ String.make 1 initial ^ "}"
  | White, Player { initial; _ } ->
      "{White, Player " ^ String.make 1 initial ^ "}"

(* let tile_w_one_king p1 = { color = Black; occupant = p1; is_king = true }
   let tile_w_no_king p2 = { color = White; occupant = p2; is_king = false }
   let king_board p1 = [ [| tile_w_one_king p1 |] ] *)

let make_starting_round p1 p2 =
  State.make_round (Board.starting_board p1 p2) p1 p2 p1

let fill_row_king (p : player) (r : row) =
  Array.map (fun x -> occupy_tile p x true) r

let testing_board (p1 : player) (p2 : player) : board =
  [
    even_row empty_white_arr |> fill_row_king p2;
    odd_row empty_black_arr |> fill_row_king p2;
    even_row empty_white_arr |> fill_row_king p2;
    odd_row empty_black_arr;
    even_row empty_white_arr;
    odd_row empty_black_arr |> fill_row_king p1;
    even_row empty_white_arr |> fill_row_king p1;
    odd_row empty_black_arr |> fill_row_king p1;
  ]

let test_round x y = State.make_round (testing_board x y) x y x

let rec print_list lst =
  match lst with [] -> "" | [ h ] -> h | h :: t -> h ^ ", " ^ print_list t

(** [starting_board_test name p1 p2 col_letter row_number expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with the tile in the starting board with player [p1] and 
    player [p2] at column [col_letter] and row [row_number]. *)
let starting_board_test (name : string) (p1 : player) (p2 : player)
    (col_letter : char) (row_number : int) (expected_output : tile) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_tile (starting_board p1 p2) col_letter row_number)
    ~printer:print_tile

(** [tile_test name board col_letter row_number expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    the tile in the board [board] at column [col_letter] and row [row_number]. *)
let tile_test (name : string) (board : Board.board) (col_letter : char)
    (row_number : int) (expected_output : tile) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_tile board col_letter row_number)
    ~printer:print_tile

(** [state_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [input]. *)
let state_test (name : string) input expected_output : test =
  name >:: fun _ -> assert_equal expected_output input ~printer:print_list

(** [eq_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [input]. 
    Intended for use for tests where printing is irrelevant or too complex. *)
let eq_test (name : string) input expected_output : test =
  name >:: fun _ -> assert_equal expected_output input

(** [int_eq_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [input]. *)
let int_eq_test (name : string) input expected_output : test =
  name >:: fun _ -> assert_equal expected_output input ~printer:string_of_int

(** [raises_test name input exn] constructs an OUnit test named
    [name] that asserts that [input] raises exception [exn]. *)
let raises_test (name : string) input exn : test =
  name >:: fun _ -> assert_raises exn input

let is_king_test (name : string) round col row expected_output : test =
  name >:: fun _ -> assert_equal expected_output (State.iss_king round col row)

let p1 = Board.create_player 'J'
let p2 = Board.create_player 'L'
let init = State.starting_round p1 p2

let king_update_test round =
  let row = 1 in
  let col = 'D' in
  State.update_king round col row;
  round

let test_ col row = State.iss_king (king_update_test init) col row

(* Add component tests such as:
   let start_test = [list of start tests]*)

(* let king_update_test =
   "iss_king (king_update init row=8 col='C') = true" >:: fun _ ->
   assert_equal true test_ *)

let _king_test (name : string) col row expected_output : test =
  name >:: fun _ -> assert_equal expected_output (test_ col row)

let board_tests =
  [
    starting_board_test
      "Player1 = 'J' and Player2 = 'L' so tile A1 is type 0, None"
      (Board.create_player 'J') (Board.create_player 'L') 'A' 1
      { color = White; occupant = None; is_king = false };
    starting_board_test
      "Player1 = 'J' and Player2 = 'L' so tile A2 is color Black, player L"
      (Board.create_player 'J') (Board.create_player 'L') 'A' 2
      { color = Black; occupant = Board.create_player 'L'; is_king = false };
    starting_board_test "Rows 4 and 5 are empty so A4 has color Black, None"
      (Board.create_player 'J') (Board.create_player 'L') 'A' 4
      { color = Black; occupant = None; is_king = false };
    starting_board_test "Rows 4 and 5 are empty so B4 has color White, None"
      (Board.create_player 'J') (Board.create_player 'L') 'B' 4
      { color = White; occupant = None; is_king = false };
    starting_board_test "Rows 4 and 5 are empty so C5 has color White, None"
      (Board.create_player 'J') (Board.create_player 'L') 'C' 5
      { color = White; occupant = None; is_king = false };
    starting_board_test "Rows 4 and 5 are empty so D5 has color Black, None"
      (Board.create_player 'J') (Board.create_player 'L') 'D' 5
      { color = Black; occupant = None; is_king = false };
    starting_board_test
      "Player1 = 'J' and Player2 = 'L' so tile A6 is color Black, player J"
      (Board.create_player 'J') (Board.create_player 'L') 'A' 6
      { color = Black; occupant = Board.create_player 'J'; is_king = false };
    starting_board_test
      "Player1 = 'J' and Player2 = 'L' so tile F8 is type 0, None"
      (Board.create_player 'J') (Board.create_player 'L') 'F' 8
      { color = White; occupant = None; is_king = false };
  ]

let init_state_tests =
  [
    eq_test "starting board is the initial board"
      (Board.starting_board p1 p2)
      (State.get_board (State.starting_round p1 p2));
    int_eq_test "p1's score starts at 0 points" (int_of_string (get_score p1)) 0;
    int_eq_test "p2's score starts at 0 points" (int_of_string (get_score p2)) 0;
    int_eq_test "p1 starts with 12 pieces" (get_num_pieces p1) 12;
    int_eq_test "p2 starts with 12 pieces" (get_num_pieces p2) 12;
    eq_test "player 1 is the current player during the starting round" p1
      (State.get_current_player init);
    state_test "a piece that can't move has an empty move list"
      (State.list_simple_moves init 'A' 8)
      [];
    raises_test
      "trying to move player 2's piece during player 1's turn raises exception \
       WrongPlayer"
      (fun () -> State.list_simple_moves init 'A' 2)
      State.WrongPlayer;
    tile_test "tile B5 is Black, None because it's an empty tile"
      (Board.starting_board p1 p2)
      'B' 5
      { color = Black; occupant = None; is_king = false };
    raises_test
      "trying to select tile B5 where there are no pieces present raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves init 'B' 5)
      State.NoPiece;
    raises_test
      "trying to select tile D4 where there are no pieces present raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves init 'D' 4)
      State.NoPiece;
    state_test "p1's piece at A6 can move to position B5"
      (State.list_simple_moves init 'A' 6)
      [ "B5" ];
    state_test "p1's piece at A6 cannot jump anywhere"
      (State.list_jump_moves init 'A' 6)
      [];
    raises_test
      "trying to select tile B3 with p2's piece in the starting board raises \
       exception WrongPlayer"
      (fun () -> State.list_simple_moves init 'B' 3)
      State.WrongPlayer;
  ]

(* separate dfn for first_move since it will be manipulated with State.move *)
let first_move =
  let first = State.make_round (Board.starting_board p1 p2) p1 p2 p1 in
  State.move first 'A' 6 'B' 5;
  State.set_current_player first p2;
  first

let first_move_state_tests =
  [
    eq_test "player 2 is the current player after 1st move"
      (State.get_current_player first_move)
      p2;
    int_eq_test "p1's score after 1 move at 0 points"
      (int_of_string (get_score p1))
      0;
    int_eq_test "p2's score after 1 move at 0 points"
      (int_of_string (get_score p2))
      0;
    int_eq_test "p1 after 1 move still has 12 pieces" (get_num_pieces p1) 12;
    int_eq_test "p2 after 1 move still has 12 pieces" (get_num_pieces p2) 12;
    state_test "a piece that still can't move (A2) has an empty move list"
      (State.list_simple_moves first_move 'A' 2)
      [];
    raises_test
      "trying to move player 1's piece during player 2's turn raises exception \
       WrongPlayer"
      (fun () -> State.list_simple_moves first_move 'A' 8)
      State.WrongPlayer;
    raises_test
      "trying to select a tile where there are no pieces present raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves first_move 'A' 1)
      State.NoPiece;
    raises_test
      "there is no piece at A6 after it got moved and it raises NoPiece"
      (fun () -> State.list_simple_moves first_move 'A' 6)
      State.NoPiece;
    raises_test
      "there is a piece at B5 belonging to p1 after being moved from A6"
      (fun () -> State.list_simple_moves first_move 'B' 5)
      State.WrongPlayer;
    state_test "the piece at tile B3 can move to tile A4 and C4"
      (State.list_simple_moves first_move 'B' 3)
      [ "A4"; "C4" ];
    state_test "the piece at tile B3 cannot jump anywhere"
      (State.list_jump_moves first_move 'B' 3)
      [];
    eq_test "initial board isn't the same as the board after the first move"
      true
      (State.get_board init <> State.get_board first_move);
  ]

(* separate dfn for snd_move since it will be manipulated with State.move *)
let snd_move =
  let snd = State.make_round (Board.starting_board p1 p2) p1 p2 p1 in
  State.move snd 'A' 6 'B' 5;
  State.set_current_player snd p2;
  State.move snd 'B' 3 'C' 4;
  State.set_current_player snd p1;
  snd

let snd_move_state_tests =
  [
    eq_test "player 1 is the current player after 2nd move"
      (State.get_current_player snd_move)
      p1;
    int_eq_test "p1's score after 2 moves at 0 points"
      (int_of_string (get_score p1))
      0;
    int_eq_test "p2's score after 2 moves at 0 points"
      (int_of_string (get_score p2))
      0;
    int_eq_test "p1 after 2 moves still has 12 pieces" (get_num_pieces p1) 12;
    int_eq_test "p2 after 2 moves still has 12 pieces" (get_num_pieces p2) 12;
    state_test "piece at B5 (moved during move 1) can move to A4"
      (State.list_simple_moves snd_move 'B' 5)
      [ "A4" ];
    raises_test
      "trying to select a tile where there used to be a piece (A6) raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves snd_move 'A' 6)
      State.NoPiece;
    state_test "a piece that still can't move (E8) has an empty move list"
      (State.list_simple_moves snd_move 'E' 8)
      [];
    raises_test
      "trying to move player 2's piece during player 1's turn raises exception \
       WrongPlayer"
      (fun () -> State.list_simple_moves snd_move 'A' 2)
      State.WrongPlayer;
    raises_test
      "trying to select a tile where there are no pieces present raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves snd_move 'A' 1)
      State.NoPiece;
    raises_test
      "there is no piece at B3 after it got moved and it raises exception \
       NoPiece"
      (fun () -> State.list_simple_moves snd_move 'B' 3)
      State.NoPiece;
    tile_test "tile C4 is Black, p2 because a piece from p2 was moved there"
      (State.get_board snd_move) 'C' 4
      { color = Black; occupant = p2; is_king = false };
    raises_test
      "there is a piece at C4 belonging to p2 after being moved from B4"
      (fun () -> State.list_simple_moves snd_move 'C' 4)
      State.WrongPlayer;
    state_test "p1's piece at B7 can move to the empty tile at A6"
      (State.list_simple_moves snd_move 'B' 7)
      [ "A6" ];
    state_test "p1's piece at B7 cannot jump anywhere"
      (State.list_jump_moves snd_move 'B' 7)
      [];
    eq_test "initial board isn't the same as the board after the second move"
      true
      (State.get_board init <> State.get_board snd_move);
    eq_test
      "first move board isn't the same as the second move board after the \
       second move"
      true
      (State.get_board first_move <> State.get_board snd_move);
  ]

(* separate dfn for third_move since it will be manipulated with State.move *)
let third_move =
  let third = State.make_round (Board.starting_board p1 p2) p1 p2 p1 in
  State.move third 'A' 6 'B' 5;
  State.set_current_player third p2;
  State.move third 'B' 3 'C' 4;
  State.set_current_player third p1;
  State.move third 'G' 6 'H' 5;
  State.set_current_player third p2;
  third

let third_move_tests =
  [
    eq_test "player 2 is the current player after 3rd move"
      (State.get_current_player third_move)
      p2;
    int_eq_test "p1's score after 3 moves at 0 points"
      (int_of_string (get_score p1))
      0;
    int_eq_test "p2's score after 3 moves at 0 points"
      (int_of_string (get_score p2))
      0;
    int_eq_test "p1 after 3 moves still has 12 pieces" (get_num_pieces p1) 12;
    int_eq_test "p2 after 3 moves still has 12 pieces" (get_num_pieces p2) 12;
    state_test "p2's piece at C4 (from first move) can move to D5"
      (State.list_simple_moves third_move 'C' 4)
      [ "D5" ];
    raises_test
      "there is p1's piece at tile B5 after the first move so it raises \
       exception WrongPlayer"
      (fun () -> State.list_simple_moves third_move 'B' 5)
      State.WrongPlayer;
    state_test "p2's piece at C4 (from first move) can jump to A6"
      (State.list_jump_moves third_move 'C' 4)
      [ "A6" ];
    raises_test
      "there is no piece at tile G6 after the third move so it raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves third_move 'G' 6)
      State.NoPiece;
    raises_test
      "there is no piece at tile B3 after the second move so it raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves third_move 'B' 3)
      State.NoPiece;
    state_test "p2's piece at A2 can now move to B3 after the second move"
      (State.list_simple_moves third_move 'A' 2)
      [ "B3" ];
    state_test "p2's piece at B1 cannot move after 3 moves"
      (State.list_simple_moves third_move 'B' 1)
      [];
    eq_test "initial board isn't the same as the board after the third move"
      true
      (State.get_board init <> State.get_board third_move);
    eq_test "first move board isn't the same as the board after the third move"
      true
      (State.get_board first_move <> State.get_board third_move);
    eq_test "second move board isn't the same as the board after the third move"
      true
      (State.get_board snd_move <> State.get_board third_move);
  ]

(* separate dfn for fourth_move since it will be manipulated with State.move *)
let fourth_move =
  let four = State.make_round (Board.starting_board p1 p2) p1 p2 p1 in
  State.move four 'A' 6 'B' 5;
  State.set_current_player four p2;
  State.move four 'B' 3 'C' 4;
  State.set_current_player four p1;
  State.move four 'G' 6 'H' 5;
  State.set_current_player four p2;
  State.jump four 'C' 4 'A' 6;
  State.set_current_player four p1;
  four

let fourth_move_tests =
  [
    eq_test "player 1 is the current player after 4th move"
      (State.get_current_player fourth_move)
      p1;
    int_eq_test "p1's score after 4 moves at 0 points"
      (int_of_string (get_score p1))
      0;
    int_eq_test "p2's score after 4 moves at 1 point"
      (int_of_string (get_score p2))
      1;
    int_eq_test "p1 after 4 moves now has 11 pieces" (get_num_pieces p1) 11;
    int_eq_test "p2 after 4 moves still has 12 pieces" (get_num_pieces p2) 12;
    raises_test
      "there is p2's piece on A6 after jumping B5 in fourth move so it raises \
       exception WrongPlayer"
      (fun () -> State.list_simple_moves fourth_move 'A' 6)
      State.WrongPlayer;
    raises_test
      "there is no piece on B5 after being jumped in fourth move so it raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves fourth_move 'B' 5)
      State.NoPiece;
    raises_test
      "there is no piece on C4 after jumping to A6 in fourth move so it raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves fourth_move 'C' 4)
      State.NoPiece;
    raises_test
      "there is no piece still on G6 after the third move so it raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves fourth_move 'G' 6)
      State.NoPiece;
    state_test "p1's piece at H7 can move to G6 (G6 empty from third move)"
      (State.list_simple_moves fourth_move 'H' 7)
      [ "G6" ];
    state_test "p1's piece at F7 can move to G6 (G6 empty from third move)"
      (State.list_simple_moves fourth_move 'F' 7)
      [ "G6" ];
    state_test "p1's piece at H5 can move to G4 (H5 filled from third move)"
      (State.list_simple_moves fourth_move 'H' 5)
      [ "G4" ];
    state_test "p1's piece at B7 cannot jump (even though p2 is at A6)"
      (State.list_jump_moves fourth_move 'H' 5)
      [];
    raises_test
      "there is p2's piece at tile B1 from the starting board so it raises \
       exception WrongPlayer"
      (fun () -> State.list_simple_moves fourth_move 'B' 1)
      State.WrongPlayer;
    raises_test
      "there is no piece at tile B3 (moved from B3 -> C4 -> A6) so it raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves fourth_move 'B' 3)
      State.NoPiece;
    raises_test
      "there is no piece at G6 (moved from G6 -> B5 during move 3) so it \
       raises exception NoPiece"
      (fun () -> State.list_simple_moves fourth_move 'G' 6)
      State.NoPiece;
    eq_test "initial board isn't the same as the board after the fourth move"
      true
      (State.get_board init <> State.get_board fourth_move);
    eq_test "first move board isn't the same as the board after the fourth move"
      true
      (State.get_board first_move <> State.get_board fourth_move);
    eq_test
      "second move board isn't the same as the board after the fourth move" true
      (State.get_board snd_move <> State.get_board fourth_move);
    eq_test "third move board isn't the same as the board after the fourth move"
      true
      (State.get_board third_move <> State.get_board fourth_move);
  ]

(* separate dfn for fifth_move since it will be manipulated with State.move *)
(* need to create "new" players since otherwise score will update twice for jump
   during move 4 *)
let new_p1 = Board.create_player 'J'
let new_p2 = Board.create_player 'L'

let fifth_move =
  let fifth =
    State.make_round (Board.starting_board new_p1 new_p2) new_p1 new_p2 new_p1
  in
  State.move fifth 'A' 6 'B' 5;
  State.set_current_player fifth new_p2;
  State.move fifth 'B' 3 'C' 4;
  State.set_current_player fifth new_p1;
  State.move fifth 'G' 6 'H' 5;
  State.set_current_player fifth new_p2;
  State.jump fifth 'C' 4 'A' 6;
  State.set_current_player fifth new_p1;
  State.move fifth 'H' 7 'G' 6;
  State.set_current_player fifth new_p2;
  fifth

let fifth_move_tests =
  [
    eq_test "player 2 is the current player after 5th move"
      (State.get_current_player fifth_move)
      new_p2;
    int_eq_test "p1's score after 5 moves still at 0 points"
      (int_of_string (get_score new_p1))
      0;
    int_eq_test "p2's score after 5 moves still at 1 point"
      (int_of_string (get_score new_p2))
      1;
    int_eq_test "p1 after 4 moves still has 11 pieces" (get_num_pieces new_p1)
      11;
    int_eq_test "p2 after 4 moves still has 12 pieces" (get_num_pieces new_p2)
      12;
    state_test
      "there is p2's piece on A6 after jumping B5 in fourth move but it cannot \
       move so its move list is empty"
      (State.list_simple_moves fifth_move 'A' 6)
      [];
    state_test
      "there is p2's piece at A6 but it cannot jump again so jump list is also \
       empty"
      (State.list_jump_moves fifth_move 'A' 6)
      [];
    raises_test
      "there is still no piece on B5 after being jumped in fourth move so it \
       raises exception NoPiece"
      (fun () -> State.list_simple_moves fifth_move 'B' 5)
      State.NoPiece;
    raises_test
      "there is now p1's piece at G6 after moving to G6 in fifth move so it \
       raises exception WrongPlayer"
      (fun () -> State.list_simple_moves fifth_move 'G' 6)
      State.WrongPlayer;
    raises_test
      "there is no piece at tile H7 after p1 moved it to G6 in fifth move so \
       it raises exception NoPiece"
      (fun () -> State.list_simple_moves fifth_move 'H' 7)
      State.NoPiece;
    state_test
      "p2's piece at C2 can move to B3 (B3 is empty from the second move)"
      (State.list_simple_moves fifth_move 'C' 2)
      [ "B3" ];
    state_test
      "p2's piece at B1 cannot move anywhere still so move list is empty"
      (State.list_simple_moves fifth_move 'B' 1)
      [];
    raises_test
      "there is no piece at tile E5 from the starting board set up so it \
       raises exception NoPiece"
      (fun () -> State.list_simple_moves fifth_move 'E' 5)
      State.NoPiece;
    raises_test
      "there is p1's piece at tile C6 from the starting board set up so it \
       raises exception WrongPlayer"
      (fun () -> State.list_simple_moves fifth_move 'C' 6)
      State.WrongPlayer;
    state_test
      "p2's piece still at D3 from starting board could move to tile C4 or E4"
      (State.list_simple_moves fifth_move 'D' 3)
      [ "C4"; "E4" ];
    eq_test "initial board isn't the same as the board after the fifth move"
      true
      (State.get_board init <> State.get_board fifth_move);
    eq_test "first move board isn't the same as the board after the fifth move"
      true
      (State.get_board first_move <> State.get_board fifth_move);
    eq_test "second move board isn't the same as the board after the fifth move"
      true
      (State.get_board snd_move <> State.get_board fifth_move);
    eq_test "third move board isn't the same as the board after the fifth move"
      true
      (State.get_board third_move <> State.get_board fifth_move);
    eq_test "fourth move board isn't the same as the board after the fifth move"
      true
      (State.get_board fourth_move <> State.get_board fifth_move);
  ]

(* tests for kings *)
let king_tests =
  [
    is_king_test
      "iss_king on the starting_board will return false since all tiles are \
       not kings to begin"
      (make_starting_round p1 p2)
      'A' 6 false;
    is_king_test "iss_king on a tile that has a king on it will return true"
      (test_round p1 p2) 'C' 6 true;
    state_test "a king that can't move has an empty move list"
      (State.list_backwards_moves init 'A' 8)
      [];
    _king_test
      "testing to see if round updates to king = true after reaches end of \
       board, row = 1"
      'D' 1 true;
    _king_test
      "calling is_king on a piece that hasnt been updated to be a king should \
       return false"
      'D' 2 false;
  ]

(* round for debugging movement/board issues with the rows.
    most recent issue: the empty rows are being occupied by a player after a move
    and aren't being empty when we create a new starting board to check a new
    move.
    solution (if any): instead of defining the row array with a constant array we
   generate a new one for each row and do not leave any rows as that literal. *)
let debug_round =
  let first = State.make_round (Board.starting_board p1 p2) p1 p2 p1 in
  State.move first 'A' 6 'B' 5;
  State.set_current_player first p2;
  first

(* tests that involve the function we're isolating to debug *)
let debug_tests =
  [
    tile_test "tile B5 is Black, None because it's an empty tile"
      (Board.starting_board p1 p2)
      'B' 5
      { color = Black; occupant = None; is_king = false };
    raises_test
      "trying to select tile B5 where there are no pieces present raises \
       exception NoPiece"
      (fun () -> State.list_simple_moves init 'B' 5)
      State.NoPiece;
    raises_test
      "there is a piece at B5 belonging to p1 after being moved from A6"
      (fun () -> State.list_simple_moves debug_round 'B' 5)
      State.WrongPlayer;
  ]

(* [switch] is false when we're only doing debugging tests, true otherwise *)
let switch = true

(* the list of tests we're using for OUnit testing, should contain full list of
   tests when switch is true *)
let test_list =
  if switch = false then
    List.flatten
      [
        (* Add test lists here
           board_tests;*)
        debug_tests;
      ]
  else
    List.flatten
      [
        (* Add test lists here *)
        board_tests;
        init_state_tests;
        first_move_state_tests;
        snd_move_state_tests;
        third_move_tests;
        fourth_move_tests;
        fifth_move_tests;
        king_tests;
        debug_tests;
      ]

let tests = "test suite for Checkers" >::: test_list
let _ = run_test_tt_main tests
