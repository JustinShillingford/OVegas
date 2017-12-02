open Card
open Table
open Player
open Command

type state = {players: player list; play_round: int; bet_round: int; pot:int; table: table; latest_bet:int; curr_player: player; message: string}

let initial_state player_list deck =
  {players = player_list; play_round = 0; bet_round = 0; pot = 0; table=(deck, None); latest_bet=0; curr_player=(List.nth player_list 0); message= ""}  

let string_of_state st =
  failwith "Unimplemented"

let is_human st =
  st.curr_player = List.nth st.players 0

(* Remove p argument from do' *)
let do' st c =
  match c with
  (* | Call -> call_helper st
  | Fold -> fold_helper st
  | Bet(i) -> bet_helper st i
  | Check -> check_helper st
  | Raise(i) -> raise_helper st i *)
  | Call -> st
  | Quit -> st
  | _ -> failwith "Unimplemented - do'"