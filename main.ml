open State
open Player
open Table
open Command
open Ai
open Ui

let rec repl st =
  if (is_human st) then begin
    print_endline ("Enter an action.");
    print_string "> ";
    let user_input = parse (read_line ()) in
    let next_state = do' st user_input in
    match user_input with
    | Call -> begin
        print_endline ("You have just Called.");
        build_table next_state;
        print_endline next_state.message;
        repl next_state
      end
    | Fold -> begin
        print_endline ("You have just Folded.");
        build_table next_state;
        print_endline next_state.message;        
        repl next_state
      end
    | Bet(i) -> begin
        print_endline ("You have just Bet $" ^ string_of_int i ^ ".");
        build_table next_state;
        print_endline next_state.message;        
        repl next_state
      end
    | Check -> begin
        print_endline ("You have Checked.");
        build_table next_state;
        print_endline next_state.message;
        repl next_state
      end
    | Raise(i) -> begin
        print_endline ("You have Raised by $" ^ string_of_int i ^ ".");
        build_table next_state;
        print_endline next_state.message;
        repl next_state
      end
    | Quit -> ()
  end
  else (* AI's turn; will only choose valid commands  *) begin
    let ai_input = ai_command st in
    let next_state = do' st ai_input in
    match ai_input with
    | Call -> begin
        print_endline ("AI has just Called.");
        build_table next_state;
        print_endline next_state.message;
        repl next_state
      end
    | Fold -> begin
        print_endline ("AI has just Folded.");
        build_table next_state;
        print_endline next_state.message;
        repl next_state
      end
    | Bet(i) -> begin
        print_endline ("AI has just Bet $" ^ string_of_int i ^ ".");
        build_table next_state;
        print_endline next_state.message;
        repl next_state
      end
    | Check -> begin
        print_endline ("AI has Checked.");
        build_table next_state;
        print_endline next_state.message;
        repl next_state
      end
    | Raise(i) -> begin
        print_endline ("AI has Raised by $" ^ string_of_int i ^ ".");
        build_table next_state;
        print_endline next_state.message;
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
  repl init_st

let () = playgame ()