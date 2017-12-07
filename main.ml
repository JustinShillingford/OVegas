open State
open Player
open Table
open Command
open Ai
open Ui

let valid_command_helper st =
  let rec valid_command_helper_ii st cmd_list acc =
    match cmd_list with
    | [] -> acc
    | h::t -> if is_valid_command st h then valid_command_helper_ii st t ((cmd_to_string h) ^ " " ^ acc)
      else valid_command_helper_ii st t acc
  in valid_command_helper_ii st [Quit; Fold; Check; Call; Raise(0); Bet(0)] ""

let rec repl st =
  if (st.message)="quit" then () else
  if (st.bet_round=5)

  then ((build_table st); (print_endline st.message);
        let reset_st =
          if ((List.length (fst st.table)) >=9)then
            new_play_round {st with bet_round=0; play_round=st.play_round+1;
                                    pot=0; latest_bet=0; message=""}
          else (new_play_round_new_deck
              ({st with bet_round=0;play_round=st.play_round+1;pot=0;
                        latest_bet=0; message=""}))
        in (repl reset_st)) else
    let st = if (st.bet_round=0) then (blinds st) else st in
    build_table st;
    print_endline st.message;
    if (st.curr_player.is_human) then begin
      print_endline ("Enter an action.");
      print_endline ("The valid commands are: " ^ valid_command_helper st);
      print_string "> ";
      let user_input = parse (read_line ())  in
      let next_state = try do' st user_input with e ->
      match e with
      | InvalidBet -> print_endline "That's an invalid bet."; st
      | InvalidCommand (c) -> ANSITerminal.(print_string [red] "That's an invalid command.\n\n"); st
      | InvalidRaise -> ANSITerminal.(print_string [red] "That's an invalid amount."); st
      | GameOver (win_id,s) -> begin
          if (win_id = "AI") then (build_table {s with bet_round=5};
                                   lose_message (); {st with message="quit"})
          else (build_table {s with bet_round=5};
                win_message (); {st with message="quit"})
        end
      | _ -> (st)
      in
      if next_state.message="quit" then ()
      else if next_state = st then (repl st) else begin
        match user_input with
        | Call -> begin
            print_endline ("You have just Called.");
            repl next_state
          end
        | Fold -> begin
            print_endline ("You have just Folded.");
            repl next_state
          end
        | Bet(i) -> begin
            print_endline ("You have just Bet $" ^ string_of_int i ^ ".");
            repl next_state
          end
        | Check -> begin
            print_endline ("You have Checked.");
            repl next_state
          end
        | Raise(i) -> begin
            print_endline ("You have Raised by $" ^ string_of_int i ^ ".");
            repl next_state
          end
        | Quit -> ()
      end
    end
    else (* AI's turn; will only choose valid commands  *) begin
      let ai_input = ai_command st in
      let next_state = try do' st ai_input with e ->
      match e with
      | GameOver (win_id,s) ->
        begin
          if (win_id = "AI") then
            (build_table {s with bet_round=5};
             lose_message (); {st with bet_round=5; message="quit"})
          else
            (build_table {s with bet_round=5};
             win_message (); {st with bet_round=5; message="quit"})
        end
      | _ -> st in
      if next_state.message="quit" then ()
      else if next_state = st then (repl st) else begin
        print_endline "AI is thinking...";
        (Unix.sleep 2);
        match ai_input with
        | Call -> begin
            print_endline ("AI has just Called.");
            repl next_state
          end
        | Fold -> begin
            print_endline ("AI has just Folded.");
            repl next_state
          end
        | Bet(i) -> begin
            print_endline ("AI has just Bet $" ^ string_of_int i ^ ".");
            repl next_state
          end
        | Check -> begin
            print_endline ("AI has Checked.");
            repl next_state
          end
        | Raise(i) -> begin
            print_endline ("AI has Raised by $" ^ string_of_int i ^ ".");
            repl next_state
          end
        | Quit -> ()
      end
    end

(* [which_diff inp] is the difficulty of game the player decides.
    Defaults to medium difficulty if the user provides invalid input. *)
let which_diff inp =
  let s = String.lowercase_ascii (String.trim inp) in
  if s = "easy" || s = "medium" || s = "hard" then s
  else "medium"

let playgame () =
  ANSITerminal.(print_string [red] "\nWelcome to OVegas, the OCaml Texas Hold'em!");
  print_endline "";
  ANSITerminal.(print_string [yellow] "
            H4sI         CK
           MhvE0AA     3RleHQu
          dHh0AG2S    MXLcMAxF
          e58CB9BoJs5MinSbIpM0         Kdyk
          5oqQiCwF7pCUFXU+hn09ny     Qf1GrdpK
        FmCAj//weelFL0tLKLNWx0Zp   8WUSqB5RJoza
        kyBSm0Sozk1JOXZ/Hs2+WYcl2  UO3IUXZ7QydlTG
      mlwM8eC+znp1FpryMxUkpaeTkOY2 Xe4Y+LouVQrd
    LQ69CUSxRiprTx         dnNonqLsIwoPCcbaiB
    LSgjgTV3ga0     OVegas        ms6Khp5+B9Z
dw     OHMy           by           EgMtMmKjp9
       ZD    Johanna      Ghassane
      8N2    Mishcat  &   Justin   sQz1sNELO
      o7pkx sd2a  ts      kjdlfkjslfjasslkd
      igzG  Zo3m  5penpim0FGP9HcKEcX2vahCFw
      F2J  La95       pKkXPk95e3nn6lFf6kB
      vr0  eN++        mbDnUgR/WFh2ZTsQ0Z
      RqC  2rL         7ejL8RfW2k   of
      6/t  8H3            gs8th
      aR    493           5ycA
      F     WA           +Hl
      Zd    OY7          w0+
      pNP    fII2       MAHk
      DKE     HdmF     r7Ut
      Y4QlQ   4uN3l    56cN
                       erYOGZ
  ");
  ANSITerminal.(print_string [cyan] "
    Texas Hold'em Poker is the world's most popular poker game, both in casinos and online. Here are the key things you need to know:
    - Every player is dealt two cards, for their eyes only
    - The dealer spreads five cards - three at once, then another, then another - which can be used by all players to make their best possible five-card hand
    - Before and after each card(s) is revealed, players take turns to bet. To stay in the hand and see the next card, all players must have put the same amount of chips in the pot as each other
    - The best poker hand wins the pot
    The commands in this game are Call, Fold, Bet, Check, Raise, and Quit. Each round, you will see what the valid commands are in that round based on the table and your remaining money.
    If you ever get to less than $20 at the beginning of a round, you automatically lose since you won't be able to make the blind bets.
    If you don't enter a valid difficulty, the game will default to Medium difficulty.
    It's a seemingly simple game, but there's tons of strategies to win. Good luck!\n");
  print_endline "Please enter your name.";
  print_string "> ";
  let player1_name = read_line () in
  (* The part above should allow for the program to obtain the names of the 2
   * players. Need to find way to use these names in st for the repl. *)
  let player1 = init_player player1_name (shuffle (new_deck ())) in
  let player2 = init_player "AI" (snd player1) in
  print_endline "Choose a difficulty: Easy, Medium, or Hard";
  print_string "> ";
  let diff_input = read_line () in
  let diff = which_diff diff_input in
  let init_st = initial_state [fst player1; fst player2] (snd player2) diff in
  (* build_table init_st; *)
  ANSITerminal.(print_string [green] ("\n\t\t\t      GAME START - " ^ (String.uppercase_ascii diff) ^ "\n"));
  repl init_st

let () = playgame ()
