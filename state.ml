open Card
open Table
open Player
open Command

exception Illegal
exception InvalidBet

type state = {players: player list; round: int; street: int; pot:int; shared_cards: card list; latest_bet:int}

let initial_state player_list deck =
  ({players = player_list; round = 0; street = 0; pot = 0; shared_cards=[]; latest_bet=0}, deck)

(*let string_of_state st =

  print_endline("You are currently on round: " ^(string_of_int st.round) ^
  " and street " ^ (string_of_int st.street) ^ ". The pot is " ^
  (string_of_int st.pot) ^ ". And the latest bet was " ^
  (string_of_int st.latest_bet) ^ ". The current shared cards in
               the middle of the table are" )
  let st_shared_cards= List.map string_of_card st.shared_cards in
  match st_shared_cards with
  |[] -> print_endline "Flop has not occured yet" ;
   |c1::c2::c3::c4::c5::[] -> print_endline (c1 ^ ", " ^ c2 ^ ", " ^ c3 ^ ", " ^ c4 ^ ", " ^ c5)
   |c1::c2::c3::c4::[] -> print_endline (c1 ^ ", " ^ c2 ^ ", " ^ c3 ^ ", " ^ c4)
  |c1::c2::c3::[] -> print_endline (c1 ^ ", " ^ c2 ^ ", " ^ c3)
  |_-> raise Illegal *)

(*[do_call st p] is the state once player [p] calls in state [st]*)
let do_call st p  =
  let p_changed_cmnd = {p with latest_command = Some "call"} in
  let new_players = List.map (fun x -> if x=p then p_changed_cmnd else x) st.players in
  {st with players=new_players}


(*[do_fold st p] is the state once player [p] folds in state [st]*)
let do_fold st p =
  let new_players = (List.filter (fun (i:player) -> i!= p)
                       st.players) in
  {st with players=new_players}


(*[do_check st p] is the state once player [p] checks in state [st]*)
let do_check st p =
  let p_changed_cmnd = {p with latest_command = Some "check"} in
  let new_players = List.map (fun x -> if x=p then p_changed_cmnd else x) st.players in
  {st with players=new_players}

(*[possible_bet p x] is a boolean representing if the player [p] is betting at
  least 0 and at most however much money they have. *)
let possible_bet p x =
  if x<0 then false
  else if x<=p.money then true else false

(*[maximum_bet st] is the maximum amount either player can bet in state [st]. *)
let maximum_bet st =
  let player1=List.nth st.players 0 in
  let player2=List.nth st.players 1 in
  if player1.money>player2.money then player2.money else player1.money


(*[do_bet st p m] is the state once player [p] bets amount [m] in state [st]*)
let do_bet st p m =
  if (m<maximum_bet st && possible_bet p m) then
    let p_changed_cmnd = {p with latest_command = Some "bet"; money=p.money-m } in
    let new_players = List.map (fun x -> if x=p then p_changed_cmnd else x) st.players in
    {st with players=new_players;  pot=st.pot +m; latest_bet=m}
  else
    raise InvalidBet

let do' st p c=
  match c with
  | Call -> do_call st p
  | Fold ->  do_fold st p
  | Bet(x)->  do_bet st p x
  | Check -> do_check st p
  | Quit -> st
