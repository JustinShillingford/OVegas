open State
open Player
open Table
open Command
open Ui

let build_table s =
  let players = s.players in
  let round = s.round in
  let table = s.table in
  let pot = s.pot in
  let human = List.hd players in
  let ai = List.nth players 1 in
  let humanCards = human.two_cards in
  let aiCards = ai.two_cards in

  if (round <= 1) then begin
    print_facedown () in
    print_no_cards
    print_two_cards humanCards.hd (List.nth humanCards 1)
  end
  else if (round == 2) then begin
    print_facedown () in
    print_three_cards
    print_two_cards humanCards.hd (List.nth humanCards 1)
  end

(* This function assumes there's only two players  *)
let next_player plist old_p =
  match plist with
  | x::y::[] -> begin
    let p1 = x in let p2 = y in
    if (old_p = p1) then p2 else p1
  end
  | _ -> failwith "johanna messed up and/or there was more than two players???"

(* TODO: Remove curr_player as argument; not a parameter for do' anymore *)
(* TODO: Print message from next_state *)
let rec repl st curr_player =
  print_endline ("Enter an action.");
  print_string "> ";
  let user_input =
    match read_line () with
    | inp -> parse inp in
      let next_state = do' st curr_player user_input in
      let next_player = (next_player st.players curr_player) in
      match user_input with
      | Call -> begin
        print_endline ("You have just Called.");

        repl next_state next_player
      end
      | Fold -> begin
        print_endline ("You have just Folded.");
        repl next_state next_player
      end
      | Bet(i) -> begin
        print_endline ("You have just Bet $" ^ string_of_int i ^ ".");
        repl next_state next_player
      end
      | Quit -> ()

 let playgame () =
  ANSITerminal.(print_string [red] "\nWelcome to OVegas, the OCaml Texas Hold'em!\n");
  print_endline "Please enter the name of the first player.";
  print_string "> ";
  let player1_name = read_line () in
  print_endline "Please enter the name of the second player.";
  print_string "> ";
  let player2_name = read_line () in
  (* The part above should allow for the program to obtain the names of the 2
   * players. Need to find way to use these names in st for the repl. *)
  let player1 = init_player player1_name (shuffle (new_deck ())) in
  let player2 = init_player player2_name (snd player1) in
  repl (initial_state [fst player1; fst player2] (snd player2)) (fst player1)

let () = playgame ()