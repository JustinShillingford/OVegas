open State
open Player
open Table
open Command
open Ai
open Ui

let rec repl st =
  let st = if (st.bet_round = 0) then (blinds st) else st in
  build_table st;
  print_endline st.message;

  if (is_human st) then begin
    print_endline ("Enter an action.");
    print_endline ("The valid commands are: ");
    
    print_string "> ";
    let user_input = parse (read_line ()) in
    let next_state = try do' st user_input with e ->
      match e with
      | InvalidBet -> print_endline "That's an invalid bet."; st
      | InvalidCommand (c) -> ANSITerminal.(print_string [red] "That's an invalid command.\n\n"); st
      | InvalidRaise -> ANSITerminal.(print_string [red] "That's an invalid amount."); st
      | GameOver (win_id) -> begin
          if (win_id = "AI") then (lose_message (); st) else (win_message (); st)
        end
      | _ -> st
    in
    if next_state = st then repl st else begin
      match user_input with
      | Call -> begin
          print_endline ("You have just Called.");
          (* print_endline "--------------------------- testingggg ---------------------------------";
          win_message (); *)
          if (next_state.play_round <> st.play_round) then 
            (ANSITerminal.(print_string [magenta] (next_state.message ^ " has won. Let's play again!\n"));
            repl next_state)
          else 
            repl next_state
        end
      | Fold -> begin
          print_endline ("You have just Folded.");
          if (next_state.play_round <> st.play_round) then 
            (ANSITerminal.(print_string [magenta] (next_state.message ^ " has won. Let's play again!\n"));
            repl next_state)
          else 
            repl next_state
        end
      | Bet(i) -> begin
          print_endline ("You have just Bet $" ^ string_of_int i ^ ".");
          if (next_state.play_round <> st.play_round) then 
            (ANSITerminal.(print_string [magenta] (next_state.message ^ " has won. Let's play again!\n"));
            repl next_state)
          else 
            repl next_state
        end
      | Check -> begin
          print_endline ("You have Checked.");
          if (next_state.play_round <> st.play_round) then 
            (ANSITerminal.(print_string [magenta] (next_state.message ^ " has won. Let's play again!\n"));
            repl next_state)
          else 
            repl next_state
        end
      | Raise(i) -> begin
          print_endline ("You have Raised by $" ^ string_of_int i ^ ".");
          if (next_state.play_round <> st.play_round) then 
            (ANSITerminal.(print_string [magenta] (next_state.message ^ " has won. Let's play again!\n"));
            repl next_state)
          else 
            repl next_state
        end
      | Quit -> ()
    end
  end
  else (* AI's turn; will only choose valid commands  *) begin
    let ai_input = ai_command st in
    let next_state = do' st ai_input in
    match ai_input with
    | Call -> begin
        print_endline ("AI has just Called.");
        if (next_state.play_round <> st.play_round) then 
          (ANSITerminal.(print_string [magenta] (next_state.message ^ " has won. Let's play again!\n"));
           repl next_state)
        else 
          repl next_state
      end
    | Fold -> begin
        print_endline ("AI has just Folded.");
        if (next_state.play_round <> st.play_round) then 
            (ANSITerminal.(print_string [magenta] (next_state.message ^ " has won. Let's play again!\n"));
            repl next_state)
          else 
            repl next_state
      end
    | Bet(i) -> begin
        print_endline ("AI has just Bet $" ^ string_of_int i ^ ".");
        if (next_state.play_round <> st.play_round) then 
            (ANSITerminal.(print_string [magenta] (next_state.message ^ " has won. Let's play again!\n"));
            repl next_state)
          else 
            repl next_state
      end
    | Check -> begin
        print_endline ("AI has Checked.");
        if (next_state.play_round <> st.play_round) then 
            (ANSITerminal.(print_string [magenta] (next_state.message ^ " has won. Let's play again!\n"));
            repl next_state)
          else 
            repl next_state
      end
    | Raise(i) -> begin
        print_endline ("AI has Raised by $" ^ string_of_int i ^ ".");
        if (next_state.play_round <> st.play_round) then 
            (ANSITerminal.(print_string [magenta] (next_state.message ^ " has won. Let's play again!\n"));
            repl next_state)
          else 
            repl next_state
      end
    | Quit -> ()
  end

let playgame () =
  ANSITerminal.(print_string [red] "\nWelcome to OVegas, the OCaml Texas Hold'em!\n");
  print_endline "Please enter your name.";
  print_string "> ";
  let player1_name = read_line () in
  (* The part above should allow for the program to obtain the names of the 2
   * players. Need to find way to use these names in st for the repl. *)
  let player1 = init_player player1_name (shuffle (new_deck ())) in
  let player2 = init_player "AI" (snd player1) in
  let init_st = initial_state [fst player1; fst player2] (snd player2) in
  build_table init_st;
  ANSITerminal.(print_string [green] "\n\t\t\t      GAME START\n");
  ANSITerminal.(print_string [cyan] "
    Texas Hold'em Poker is the world's most popular poker game, both in casinos and online. Here are the key things you need to know:
    - Every player is dealt two cards, for their eyes only
    - The dealer spreads five cards - three at once, then another, then another - which can be used by all players to make their best possible five-card hand
    - Before and after each card(s) is revealed, players take turns to bet. To stay in the hand and see the next card, all players must have put the same amount of chips in the pot as each other
    - The best poker hand wins the pot
    It's a seemingly simple game, but there's tons of strategies to win. Good luck! 
  ");  
  repl init_st

let () = playgame ()