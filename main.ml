open State
open Player
open Table

let rec repl st str =
  print_endline (State.string_of_state st);
  print_string "> ";


 let playgame () =
  ANSITerminal.(print_string [red] "\nWelcome to OVegas, the OCaml Texas Hold'em!");
  print_endline "Please enter the name of the first player.";
  print_string "> ";
  let player1_name = read_line () in
  print_endline "Please enter the name of the second player.";
  print_string "> ";
  let player2_name = read_line () in
  (* The part above should allow for the program to obtain the names of the 2
   * players. Need to find way to use these names in st for the repl. *)
  let player1 = init_player player1_name shuffle(new_deck ()) in
  let player2 = init_player player2_name (snd player1) in
  repl (initial_state [fst player1; fst player2] (snd player2)) (fst player1)

(* let () = playgame () *)