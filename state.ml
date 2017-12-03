open Card
open Table
open Player
open Command

exception Illegal
exception InvalidBet
exception InvalidRaise
exception Invalid
exception Tie
exception GameOver of string
exception InvalidCommand of command

type state = {players: player list; play_round: int; bet_round: int; pot:int;
              table: table; latest_bet:int; curr_player: player; message: string;
              first_action: bool; latest_st_command: string option}

let initial_state player_list deck =
  {players = player_list; play_round = 0; bet_round = 0; pot = 0; table=(deck, None);
   latest_bet=0; curr_player=(List.nth player_list 0); message= "";first_action=true;
   latest_st_command=None}

let string_of_state st =
  failwith "Unimplemented"

let is_human st =
  st.curr_player = List.nth st.players 0

(*[make_flop_cards st] returns an updated state once the initial flop occurs*)
let make_flop_cards st =
  let (new_deck1, new_shared_cards1) = flip_new_card (st.table) in
  let (new_deck2, new_shared_cards2) = flip_new_card (new_deck1, new_shared_cards1) in
  let new_table = flip_new_card (new_deck2, new_shared_cards2) in
  {st with table = new_table}

(*[make_turn_or_river_card st] returns an updated state once the 4th or 5th card of the
  shared cards is flipped onto the table*)
let make_turn_or_river_card st =
  let new_table = flip_new_card st.table  in
  {st with table = new_table; latest_bet=0}

(*if only one player is remaining then you do round over *)
(*game over could be raised from the loop and in that case you want to quit game!*)
let round_over st = st
(*   let p1 = List.nth st.players 0 in
  let p2 = List.nth st.players 1 in
  try
    begin
      let winning_player = if (p1.remaining_in_round=false) then p2
        else if (p2.remaining_in_round=false) then p1
        else game_best_player_hand p1 p2 st in
      let p1_won_money = if p1 = winning_player then st.pot else 0 in
      let new_p1 = {p1 with money=p1.money + p1_won_money; latest_command=None; remaining_in_round=true} in
      let p2_won_money = if p2=winning_player then st.pot else 0 in
      let new_p2 = {p2 with money=p2.money + p2_won_money; latest_command=None; remaining_in_round=true} in

      if (new_p1.money<10 || new_p1.money<0) then
        raise (GameOver new_p2.id)
      else if (new_p2.money<20 || new_p2.money< 0) then
        raise (GameOver new_p1.id)

      else
        let new_player_list = [new_p1; new_p2] in
        let new_curr_player = List.nth st.players 0 in
        let new_st = {st with players = new_player_list; bet_round=0; play_round=st.play_round+1; pot=0; latest_bet=0; curr_player=new_curr_player} in
        let new_st_with_table = new_play_round_st(new_st) in
        new_st_with_table
    end
  with
  |Tie ->
    let new_p1 = {p1 with money=p1.money + (st.pot/2); latest_command=None; remaining_in_round=true} in
    let new_p2  = {p2 with money=p2.money + (st.pot/2);  latest_command=None; remaining_in_round=true} in
    if (new_p1.money<10 || new_p1.money<0) then
      raise (GameOver new_p2.id)
    else if (new_p2.money<20 || new_p2.money< 0) then
      raise (GameOver new_p1.id)

    else
      let new_player_list = [new_p1; new_p2] in
      let new_curr_player = List.nth st.players 0 in
      let new_st = {st with players = new_player_list; bet_round=0; play_round=st.play_round+1; pot=0; latest_bet=0; curr_player=new_curr_player} in
      let new_st_with_table = new_play_round_st(new_st) in
      new_st_with_table *)

let cmd_ends_betting_round st c =
  match c with
  | Call -> true
  | Check -> not st.first_action
  |_ -> false

(* This function assumes there's only two players  *)
let next_player st =
  match st.players with
  | p1::p2::[] -> begin
      if (st.curr_player.id = p1.id) then p2 else p1
    end
  | _ -> failwith "johanna messed up and/or there was more than two players???"

(*[do_call st p] is the state once player [p] calls in state [st]*)
let do_call st  =
  print_endline "here";
  print_endline (string_of_int (st.curr_player).money);
  print_endline (string_of_int st.latest_bet);
  print_endline (string_of_int (st.curr_player).money_in_pot);
  let p_changed = {st.curr_player with latest_command = Some "call";
                  money_in_pot=st.latest_bet;
                  money=(st.curr_player).money-((next_player st).money_in_pot-(st.curr_player).money_in_pot)} in
  let new_players = List.map (fun x -> if x=st.curr_player then p_changed else x) st.players in
  {st with players=new_players;  curr_player=next_player st;  first_action=false;
  latest_st_command= Some "call"; pot=st.pot+((next_player st).money_in_pot-st.curr_player.money_in_pot)}

(*[do_check st p] is the state once player [p] checks in state [st]*)
let do_check st =
  let p_changed = {st.curr_player with latest_command = Some "check"} in
  let new_players = List.map (fun x -> if x=st.curr_player then p_changed else x) st.players in
  {st with players=new_players;  curr_player=next_player st; first_action=false; latest_st_command= Some "check"}

(*[do_fold st p] is the state once player [p] folds in state [st]*)
let do_fold st =
  let other_player = next_player st in
  let other_player_changed = {other_player with money=other_player.money+st.pot} in
  print_endline ("here"^(string_of_int other_player_changed.money));
  let new_players = List.map (fun x -> if x.id=other_player_changed.id then other_player_changed else x) st.players in
  (* {(initial_state new_players (shuffle (new_deck()))) with curr_player=other_player_changed} *)
    {st with players=new_players; play_round = st.play_round + 1; bet_round=0; pot=0; table=((shuffle (new_deck())), None);
           latest_bet=0; curr_player= other_player_changed; first_action=true; latest_st_command = Some "fold"}

(*[possible_bet p x] is a boolean representing if the player [p] is betting at
  least 0 and at most however much money they have. *)
let possible_bet p x =
  if x<0 then false
  else if x<=p.money then true else false

(*[do_bet st p m] is the state once player [p] bets amount [m] in state [st]*)
let do_bet st m =
  if (possible_bet st.curr_player m) then
    let p_changed = {st.curr_player with latest_command = Some "bet"; money=(st.curr_player).money-m; money_in_pot=m} in
    let new_players = List.map (fun x -> if x.id=st.curr_player.id then p_changed else x) st.players in
    {st with players=new_players;  pot=st.pot+m; latest_bet=m; curr_player=next_player st; first_action=false;
    latest_st_command= Some "bet"}
  else
    raise InvalidBet

let possible_raise p x =
  if x<0 then false
  else if x<=(p.money) then true else false

let possible_raise2 st x =
  ((next_player st).money_in_pot-st.curr_player.money_in_pot)<x


let do_raise st m =
  (print_endline (string_of_bool (possible_raise st.curr_player m  && possible_raise2 st m)));
  if (possible_raise st.curr_player m  && possible_raise2 st m) then
    let p_changed = {st.curr_player with latest_command = Some "raise";
                    money=(st.curr_player).money- m; money_in_pot=(st.curr_player).money_in_pot+m } in
    let new_players = List.map (fun x -> if x.id=st.curr_player.id then p_changed else x) st.players in
    {st with players=new_players;  pot=st.pot +m; latest_bet=m;
    curr_player=next_player st; first_action=false; latest_st_command= Some "raise"}
  else
    raise InvalidRaise

(*valid commands*)
let is_valid_command st c=
  if st.bet_round =1 then (
    match c with
    |Call -> true
    |Raise(x) -> true
    |Fold -> true
    |Check -> false
    |Quit -> true
    |_ -> false )
  else if st.first_action then (
    match c with
    |Check -> true
    |Bet(x) -> true
    |Fold -> true
    |Quit -> true
    |_-> false )
  else
    match c with
    |Call -> true
    |Raise(x) -> true
    |Fold -> true
    |Check -> st.latest_st_command = Some "check"
    |Quit -> true
    |_-> false

(* Remove p argument from do' *)
let do' st c =
  if (is_valid_command st c)
  then
    begin
    let new_st = match c with
    (*
    | Bet(i) -> bet_helper st i
    | Raise(i) -> raise_helper st i *)
    | Raise(i) -> do_raise st i
    | Bet(i) -> do_bet st i
    | Fold -> do_fold st
    | Check -> do_check st
    | Call -> do_call st
    | Quit -> {st with message="Quit"}
    | _ -> failwith "Unimplemented - do'" in
    if (cmd_ends_betting_round st c)
    then let new_st = (if new_st.bet_round=1 then make_flop_cards new_st
                  else if (new_st.bet_round=2 || new_st.bet_round=3 || new_st.bet_round=4) then make_turn_or_river_card new_st
                  else (round_over new_st)) in
        {new_st with bet_round = new_st.bet_round +1;
        first_action = true; latest_st_command =None} (*curr player also maybe??*)
    else new_st
    end
  else
    raise (InvalidCommand c)

(*[blinds st] returns the new state after p1 and p2 have put in their small and big blinds*)
let blinds st =
  if (st.play_round mod 2) = 0
  then
    begin
    match st.players with
    |p1::p2::[] ->
      (let m1= p1.money - 10 in (*small blind is 10*)
       let m2= p2.money - 20 in (*big blind is 20*)
       let new_p1 = {p1 with money=m1; money_in_pot=10} in
       let new_p2 = {p2 with money=m2; money_in_pot=20} in
       let new_players_list = new_p1::new_p2::[] in
       let new_st = {st with players= new_players_list; pot=st.pot + 10 + 20;
                    bet_round=st.bet_round + 1; latest_bet=20; first_action=true; curr_player=new_p1} in
       new_st)
    |_-> st
    end
  else
    begin
    match st.players with
    |p1::p2::[] ->
      (let m1= p1.money - 20 in (*small blind is 10*)
       let m2= p2.money - 10 in (*big blind is 20*)
       let new_p1 = {p1 with money=m1; money_in_pot=20} in
       let new_p2 = {p2 with money=m2; money_in_pot=10} in
       let new_players_list = new_p1::new_p2::[] in
       let new_st = {st with players= new_players_list; pot=st.pot + 10 + 20;
                    bet_round=st.bet_round + 1; latest_bet=20; first_action=true; curr_player=new_p2} in
       new_st)
    |_-> st
    end
