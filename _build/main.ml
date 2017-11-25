open State
open Player
open Command (***************)

(* This function assumes there's only two players  *)
let next_player plist old_p = 
  match plist with 
  | x::y::[] -> begin
    let p1 = x in let p2 = y in
    if (old_p = p1) then p2 else p1
  end
  | _ -> failwith "johanna messed up and/or there was more than two players???"

let rec repl st curr_player =
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
        print_endline ("You have just Bet.");
        repl next_state next_player
      end
      | Quit -> ()

 let playgame () =
  failwith "playgame"

(* let () = playgame () *)