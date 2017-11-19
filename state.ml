
open Player
open Table

type state = {players: player list; round: int; pot:int }

let initial_state player_list = {players= player_list; round=0; pot=0}

(* let do_call st p  =

let do_fold st p =

let do_bet st p x = *)

let do' st p c=
  match c with
  |Call -> do_call st p
  |Fold ->  do_fold st p
  |Bet(x)->  do_bet st p x
  |Quit -> st
