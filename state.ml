open Card
open Table
open Player
open Command

exception Illegal
exception InvalidBet
exception InvalidRaise
exception Invalid
exception Tie
exception InvalidCommand of command

type state = {players: player list; play_round: int; bet_round: int; pot:int;
              table: table; latest_bet:int; curr_player: player; message: string;
              first_action: bool; latest_st_command: string option}

exception GameOver of string*state

let initial_state player_list deck =
  {players = player_list; play_round = 0; bet_round = 0; pot = 0; table=(deck, None);
   latest_bet=0; curr_player=(List.nth player_list 0); message= "";first_action=true;
   latest_st_command=None}

(* let command_to_string c =
  match c with
  |Call -> "call"
  |Raise(x) -> "raise"
  |Fold -> "fold"
  |Check ->"check"
  |Quit -> "quit"
  |Bet(x) -> "bet" *)

(* [card_list_wo_options c_option_list] is a helper function that returns the card list
   without the options*)
let card_list_wo_options c_option_list =
  match c_option_list with
  |None -> []
  |Some x -> x

let string_of_state st =
  let state_string = "You are currently on round: " ^(string_of_int st.play_round) ^
                     " and bet round " ^ (string_of_int st.bet_round) ^ ". The pot is " ^
                     (string_of_int st.pot) ^ ". The current shared cards in
                     the middle of the table are" in
  let shared_cards = card_list_wo_options (snd st.table) in
  let st_shared_cards= List.map string_of_card shared_cards in
  let state_string2=
    match st_shared_cards with
    |[] ->"Flop has not occured yet" ;
    |c1::c2::c3::c4::c5::[] -> c1 ^ ", " ^ c2 ^ ", " ^ c3 ^ ", " ^ c4 ^ ", " ^ c5
    |c1::c2::c3::c4::[] ->c1 ^ ", " ^ c2 ^ ", " ^ c3 ^ ", " ^ c4
    |c1::c2::c3::[] -> c1 ^ ", " ^ c2 ^ ", " ^ c3
    |_-> raise Illegal in
  state_string ^ state_string2

(*[compare_cards c1 c2] compares the ranks of 2 cards and returns 1 if c1's rank
  is greater than c2's, -1 if its less, or 0 if they are equivalent*)
let compare_cards c1 c2 =
  if (fst c1) > (fst c2) then 1
  else if (fst c2) > (fst c1) then -1
  else 0

(*[sorted_ranks_list card_list] returns a list of cards sorted by their ranks*)
let sorted_ranks_list card_list =
  let sorted_cards = List.sort compare_cards card_list in
  let ranks_of_sorted = List.map (fun x -> fst x) sorted_cards in
  ranks_of_sorted

(*[suits_list card_list] returns a list of the suits of cards in [card_list]*)
let suits_list card_list =
  let list_of_suits = List.map (fun x -> snd x) card_list in
  list_of_suits

(*[highest_rank card_list] returns the highest card in a hand of 5 cards*)
let highest_rank card_list =
  let ranks_list = sorted_ranks_list card_list in
  match ranks_list with
  |r1::r2::r3::r4::r5::[] -> r5
  |_ -> raise Invalid

(*[has_high_card card_list] returns tuple consisting of 1 and the list in desc order *)
let has_high_card card_list =
  let ranks_list = sorted_ranks_list card_list in
  let desc_order = List.rev ranks_list in
  Some (1, desc_order)

(*[has_four_kind card_list] returns None or a tuple consisting of 8 and the list such that the
  4 cards of same rank are close to front of list, and the other card is last element*)
let has_four_kind card_list =
  let ranks_list = sorted_ranks_list card_list in
  match ranks_list with
  |r1::r2::r3::r4::r5::[] -> if ((r1=r2) && (r2=r3) && (r3=r4)) then
      Some (8,(r1::r2::r3::r4::r5::[]))
    else if ((r2=r3) && (r3=r4) && (r4=r5)) then Some (8,(r2::r3::r4::r5::r1::[]))
    else None
  |_ -> raise Invalid

(*[has_three_kind card_list] returns None or a tuple consisting of 4 and the list such that the
  3 cards of same rank are close to front of list, and the other cards follow in
  ascending order*)
let has_three_kind card_list =
  let ranks_list = sorted_ranks_list card_list in
  match ranks_list with
  |r1::r2::r3::r4::r5::[] -> if ((r1=r2) && (r2=r3)) then Some (4,(r1::r2::r3::r4::r4::[]))
    else if ((r2=r3) && (r3=r4)) then Some (4,(r2::r3::r4::r5::r1::[]))
    else if ((r3=r4) && (r4=r5)) then Some (4,(r3::r4::r5::r2::r1::[]))
    else None
  |_ -> raise Invalid

(*[has_straight card_list] returns None or a tuple consisting of 5 and the 5 cards in
  ascending order*)
let has_straight c_list =
  let ranks_list = sorted_ranks_list c_list in
  match ranks_list with
  |r1::r2::r3::r4::r5::[]-> if ((r1=r2 -1) && (r2=r3 -1) &&(r3=r4 -1)&&(r4=r5 -1)) then
      Some (5,(r5::r4::r3::r2::r1::[]))
    else if ((r1=2) && (r2=3) && (r3=4) && (r4=5) &&(r5=14)) then
      Some (5,(r4::r3::r2::r1::r5::[]))
    else None
  |_ -> None (*not sure if this is good design choice or if i should raise invalid??*)

(*[has_flush card_list] returns None or a tuple consisting of 6 and the 5 cards in
  descending order*)
let has_flush c_list =
  let suit_list = suits_list c_list in
  match suit_list with
  |s1::s2::s3::s4::s5::[]-> if ((s1=s2) && (s2=s3) && (s3=s4) && (s4=s5)) then
      let ranks_list = List.rev (sorted_ranks_list c_list) in Some (6,ranks_list) else None
  |_-> None

(*[has_straight_flush card_list] returns None or a tuple consisting of 9 and the 5 cards in
  descending order*)
let has_straight_flush c_list =
  let check_if_straight = has_straight c_list in
  let check_if_flush = has_flush c_list in
  if (check_if_straight != None  && check_if_flush != None ) then
    let ranks_list = List.rev (sorted_ranks_list c_list) in Some (9,ranks_list) else None

(*[has_royal_flush card_list] returns None or a tuple consisting of 10 and the 5 cards in
  descending order*)
let has_royal_flush c_list =
  let check_if_straight = has_straight c_list in
  let check_if_flush = has_flush c_list in
  let check_highest_rank = highest_rank c_list in
  (*may need to change check_highest_rank=14*)
  if (check_if_straight != None && check_if_flush != None && check_highest_rank=14) then
    let ranks_list = List.rev (sorted_ranks_list c_list) in Some (10,ranks_list) else None

(*[has_full_house card_list] returns None or a tuple consisting of 10 and the 5 cards in
  descending order*)
let has_full_house c_list =
  let ranks_list = sorted_ranks_list c_list in
  match ranks_list with
  |r1::r2::r3::r4::r5::[] -> if ((r1=r2) && (r2=r3) && (r4=r5)) then
      Some (7,(r1::r2::r3::r4::r5::[]))
    else if ((r3=r4) && (r4=r5) && (r1=r2)) then
      Some (7,(r3::r4::r5::r1::r2::[]))
    else None
  |_ -> raise Invalid

(*[num_same_rank c_list acc] returns the number of pairs in a rank list*)
let rec num_same_rank r_list acc =
  match r_list with
  |[]-> acc
  |h::t-> if List.mem h t then num_same_rank t (acc+1)  else num_same_rank t acc

(*[num_same_rank_highest c_list acc] returns the highest pair in a rank list*)
let rec num_same_rank_highest r_list acc =
  match r_list with
  |[]-> acc
  |h::t-> if List.mem h t then num_same_rank_highest t h  else num_same_rank_highest t acc

(*[same_rank_lst_two_pair c_list acc] returns a list of the pairs in a rank list*)
let rec same_rank_lst_two_pair r_list acc =
  match r_list with
  |[]-> acc
  |h::t-> if List.mem h t then same_rank_lst_two_pair t (h::h::acc)  (*put the rank twice since it's a pair*)
    else same_rank_lst_two_pair t acc

(*[has_pair_helper p_rank ranks_lst] takes the pair in the original [ranks_lst] and
  returns a list with same elements in r_list with order such that pair is left-most and remaining
  ranks are in descending order*)
let has_pair_helper p_rank ranks_lst =
  let initial_list = p_rank::p_rank::[] in
  let has_pair_list_helper = List.filter (fun (x) -> (List.mem x initial_list)=false) ranks_lst in
  let ordered_has_pair_list = List.rev (List.sort compare has_pair_list_helper) in
  initial_list@ordered_has_pair_list

(*[has_pair c_list] returns None or a tuple consisting of 2 and the 5 cards in
  an order such that pair is left-most and remaining ranks are in descending order*)
let has_pair c_list =
  let ranks_list = sorted_ranks_list c_list in
  if (has_three_kind c_list != None || has_four_kind c_list != None ||
      has_full_house c_list != None) then None
  else
    let num_pairs = num_same_rank ranks_list 0 in
    let pair_rank = num_same_rank_highest ranks_list 0 in
    let has_pair_ordered_list = has_pair_helper pair_rank ranks_list in
    if num_pairs =1 then Some (2,has_pair_ordered_list) else None

(*[has_pair_helper p_rank ranks_lst] takes a list of pairs in the original [ranks_lst] and
  returns a list with same elements in r_list with order such that pairs are left-most
  in descending order and remaining rank is rightmost*)
let has_two_pair_helper p_list r_list =
  let initial_list = List.rev (List.sort compare p_list) in
  let remaining_element = List.filter (fun (x) -> (List.mem x initial_list)=false) r_list in
  initial_list@remaining_element

(*[has_two_pair c_list] returns None or a tuple consisting of 3 and the 5 cards in
  an order such that pairs are left-most in descending order and remaining rank is rightmost*)
let has_two_pair c_list =
  let ranks_list = sorted_ranks_list c_list in
  if (has_three_kind c_list != None || has_four_kind c_list != None ||
      has_full_house c_list != None) then None
  else
    let num_pairs = num_same_rank ranks_list 0 in
    let pairs_list = same_rank_lst_two_pair ranks_list []  in
    let has_pair_ordered_list = has_two_pair_helper pairs_list ranks_list in
    if num_pairs =2 then Some (3,has_pair_ordered_list) else None

(*[all_card_combinations k c_list] returns all possible combinations of k elements from
  list [c_list]
  Referenced:
  https://codereview.stackexchange.com/questions/40366/combinations-of-size-k-from-a-list-in-ocaml*)
let rec all_card_combinations k c_list =
  if k = 0 then
    [[]]
  else
    let rec all_combs_helper = function
      | []      -> []
      | h :: t -> List.map (fun x -> h :: x) (all_card_combinations (k - 1) t) :: all_combs_helper t in
    List.concat (all_combs_helper c_list)

(*[all_players player st] is a list of all possible 5-card combinations for the player*)
let all_player_cards player st =
  let p_hand = player.two_cards in
  match (snd st.table) with
  |Some (c1::c2::c3::c4::c5::[]) -> all_card_combinations 5 (c1::c2::c3::c4::c5::[]@p_hand)
  |_ -> raise Invalid

(*[highest_hand c_list c_list] returns a tuple option with the best of the 10 types of
  poker hands and the list of cards creating that best type*)
let highest_hand c_list =
  if has_royal_flush c_list != None then
    has_royal_flush c_list
  else if has_straight_flush c_list != None then
    has_straight_flush c_list
  else if has_four_kind c_list != None then
    has_four_kind c_list
  else if has_full_house c_list != None then
    has_full_house c_list
  else if has_flush c_list != None then
    has_flush c_list
  else if has_straight c_list != None then
    has_straight c_list
  else if has_three_kind c_list != None then
    has_three_kind c_list
  else if has_two_pair c_list != None then
    has_two_pair c_list
  else if has_pair c_list != None then
    has_pair c_list
  else has_high_card c_list

let option_to_list lst =
  match lst with
  | None -> raise Invalid
  | Some x -> x

(*[is_p1_best_hand p1_cards p2_cards] returns true if p1_hand beats p2_hand.*)
let is_p1_best_hand p1_cards p2_cards =
    let p1_hand = option_to_list p1_cards in
    let p2_hand= option_to_list p2_cards in
    let p1_first_rank = List.nth (snd p1_hand) 0 in
    let p2_first_rank = List.nth (snd p2_hand) 0 in
    let p1_second_rank = List.nth (snd p1_hand) 1 in
    let p2_second_rank = List.nth (snd p2_hand) 1 in
    let p1_third_rank = List.nth (snd p1_hand) 2 in
    let p2_third_rank = List.nth (snd p2_hand) 2 in
    let p1_fourth_rank = List.nth (snd p1_hand) 3 in
    let p2_fourth_rank = List.nth (snd p2_hand) 3 in
    let p1_fifth_rank= List.nth (snd p1_hand) 4 in
    let p2_fifth_rank = List.nth (snd p2_hand) 4 in
    if fst p1_hand = fst p2_hand then
      if p1_first_rank=p2_first_rank then
        if p1_second_rank = p2_second_rank then
          if p1_third_rank =p2_third_rank then
            if p1_fourth_rank=p2_fourth_rank then
              if p1_fifth_rank = p2_fifth_rank then
                raise Tie
              else p1_fifth_rank > p2_fifth_rank
            else p1_fourth_rank>p2_fourth_rank
          else p1_third_rank >p2_third_rank
        else p1_second_rank > p2_second_rank
      else  p1_first_rank>p2_first_rank
    else fst p1_hand > fst p2_hand

(*[compare_for_sort_hands hand1 hand2] returns 1 is hand1 is better than hand2,
  -1 if its worse, or 0 if hand1 and hand2 are tied*)
let compare_for_sort_hands hand1 hand2 =
  try
    begin
      if is_p1_best_hand (highest_hand hand1) (highest_hand hand2) then 1
      else if is_p1_best_hand (highest_hand hand2) (highest_hand hand1) then -1
      else 0
    end
  with
  |Tie -> 0 (*very important catch in case there is a tie among the combination of a player's hands*)

(*[player_best_hand player st] returns the best hand of the 21 different combinations
  a player can make with his 2 cards and the shared cards on the table at state [st]*)
let player_best_hand player st =
  let all_poss_five_cards = all_player_cards player st in
  let sorted_combs = List.sort compare_for_sort_hands (all_poss_five_cards) in
  let p_best_hand = List.nth sorted_combs ((List.length sorted_combs) -1) in
  p_best_hand

let game_best_player_hand p1 p2 st=
  let best_p1_cards = player_best_hand p1 st in
  let best_p2_cards =  player_best_hand p2 st in
  match compare_for_sort_hands (best_p1_cards) (best_p2_cards) with
  |1 -> p1
  | (-1) -> p2
  |_ -> raise Tie

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

(* This function assumes there's only two players  *)
let next_player st =
  match st.players with
  | p1::p2::[] -> begin
      if (st.curr_player.id = p1.id) then p2 else p1
    end
  | _ -> failwith "johanna messed up and/or there was more than two players???"

(*makes a new round after a showdown*)
let new_play_round st =
  let new_deck = shuffle(new_deck()) in
  let p1 = List.nth st.players 0 in
  let p2 = List.nth st.players 1 in
  let ((t, _), c1, c2) = make_hand new_deck in
  let ((t2, _), c3, c4) = make_hand t in
  let new_p1={p1 with two_cards = c1::c2::[]} in
  let new_p2={p2 with two_cards = c3::c4::[]} in
  let new_player_list = new_p1::new_p2::[] in
  let new_st = {st with players = new_player_list; table=(t2, None)} in
  new_st

(*if only one player is remaining then you do round over *)
(*game over could be raised from the loop and in that case you want to quit game!*)
let round_over st =
  let p1 = List.nth st.players 0 in
  let p2 = List.nth st.players 1 in
  try
    begin
      let winning_player =  game_best_player_hand p1 p2 st in
      let p1_won_money = if p1 = winning_player then st.pot else 0 in
      let new_p1 = {p1 with money=p1.money + p1_won_money; latest_command=None; remaining_in_round=true} in
      let p2_won_money = if p2=winning_player then st.pot else 0 in
      let new_p2 = {p2 with money=p2.money + p2_won_money; latest_command=None; remaining_in_round=true} in

      let s = {st with players=[new_p1;new_p2]; pot=0} in
      if (new_p1.money<20) then
        raise (GameOver (new_p2.id,s))
      else if (new_p2.money<20) then
        raise (GameOver (new_p1.id,s))

      else
        let new_player_list = [new_p1; new_p2] in
        let st = {st with players = new_player_list} in
        let new_curr_player =next_player st in
        let new_st = {st with play_round=st.play_round+1; curr_player=new_curr_player; first_action=true; latest_st_command=None} in
        {new_st with message = winning_player.id ^ " won the round!"}
    end
  with
  |Tie ->
    let new_p1 = {p1 with money=p1.money + (st.pot/2); latest_command=None; remaining_in_round=true} in
    let new_p2  = {p2 with money=p2.money + (st.pot/2);  latest_command=None; remaining_in_round=true} in
    let new_player_list = [new_p1; new_p2] in
    let st = {st with players = new_player_list} in
    let new_curr_player =next_player st in
    let new_st = {st with play_round=st.play_round+1; curr_player=new_curr_player;
    first_action=true; latest_st_command=None} in
    {new_st with message = "There was a TIE between you and AI in this round!"}


let cmd_ends_betting_round st c =
  match c with
  | Call -> true
  | Check -> not st.first_action
  |_ -> false

(*[do_call st p] is the state once player [p] calls in state [st]*)
let do_call st  =
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
  let new_players = List.map (fun x -> if x.id=other_player_changed.id then other_player_changed else x) st.players in
  (* {(initial_state new_players (shuffle (new_deck()))) with curr_player=other_player_changed} *)
  let new_st = {st with players=new_players; play_round = st.play_round + 1; bet_round=0; pot=0; table=((shuffle (new_deck())), None);
           latest_bet=0; curr_player= other_player_changed; first_action=true; latest_st_command = Some "fold"} in
  if (st.curr_player.money<20) then raise (GameOver (other_player_changed.id,new_st)) else
  new_play_round new_st

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
  if (possible_raise st.curr_player m  && possible_raise2 st m) then
    let p_changed = {st.curr_player with latest_command = Some "raise";
                    money=(st.curr_player).money- m; money_in_pot=(st.curr_player).money_in_pot+m } in
    let new_players = List.map (fun x -> if x.id=st.curr_player.id then p_changed else x) st.players in
    {st with players=new_players;  pot=st.pot +m; latest_bet=m;
    curr_player=next_player st; first_action=false; latest_st_command= Some "raise"}
  else
    if m=(-1) then (raise (InvalidCommand (Raise (-1)))) else (raise InvalidRaise)

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
    | Raise(i) -> do_raise st i
    | Bet(i) -> do_bet st i
    | Fold -> do_fold st
    | Check -> do_check st
    | Call -> do_call st
    | Quit -> {st with message="Quit"} in
    if (cmd_ends_betting_round st c)
    then let new_st = (if new_st.bet_round=1 then make_flop_cards new_st
                      else if (new_st.bet_round=2 || new_st.bet_round=3) then make_turn_or_river_card new_st
                      else (round_over new_st)) in
    let new_players = List.map (fun p -> {p with money_in_pot=0; latest_command=None}) new_st.players in
        {new_st with bet_round = new_st.bet_round +1;
        first_action = true; latest_st_command =None; players=new_players} (*curr player also maybe??*)
    else new_st
    end
  else
    (print_endline "here";
    raise (InvalidCommand c))

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
