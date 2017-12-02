open State
open Player
open Table
open Card
open Command

let ai_command st=
  Call



let rec repl st =
  (*print current state of the table ->ascii*)
  (*print_state st*)
  (*
  if round=0
    then
    print betting the blind
    new_st = blinds st
    print_state new_st
*)

  (*if round =1
    then
    new_st = fst_betting_round st 


  *)


  (* look at round_number to see what function should be called *)

  let user_input = if (is_human st st.curr_player) then
      ( print_endline ("Enter an action."); print_string "> "; parse (read_line ()))
      else ai_command st in

  (*let next_state = do' st st.curr_player user_input in *)
  let () = print_endline(st.message) in
  match user_input with
      | Call -> begin
        print_endline ("You have just Called.");
        repl next_state
      end
      | Fold -> begin
        print_endline ("You have just Folded.");
        repl next_state
      end
      | Raise(i) -> begin
        print_endline ("You have just Raise $" ^ string_of_int i ^ ".");
        repl next_state
      end
      | Check -> begin
        print_endline ("You have just Checked.");
        repl next_state
      end
      | Bet(i) -> begin
        print_endline ("You have just Bet $" ^ string_of_int i ^ ".");
        repl next_state
      end
      | Quit -> ()



 let playgame () =
  ANSITerminal.(print_string [red] "\nWelcome to OVegas, the OCaml Texas Hold'em!\n");
  print_endline "Please enter the name of the first player.";
  print_string "> ";
  let player1_name = read_line () in
  let player2_name = "AI" in
  (* The part above should allow for the program to obtain the names of the 2
   * players. Need to find way to use these names in st for the repl. *)
  let player1 = init_player player1_name (shuffle (new_deck ())) in
  let player2 = init_player player2_name (snd player1) in
  repl (initial_state [fst player1; fst player2] (snd player2))

let () = playgame ()
