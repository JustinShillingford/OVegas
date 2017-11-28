open Card
open Table
open Player
open Command

exception Illegal
exception InvalidBet
exception Invalid
exception Tie

type state = {players: player list; round: int; street: int; pot:int; table: table; latest_bet:int}

let initial_state player_list deck =
  {players = player_list; round = 0; street = 0; pot = 0; table=(deck, None); latest_bet=0}

let make_flop_cards st =
  let (new_deck1, new_shared_cards1) = flip_new_card (fst st.table, snd st.table) in
  let (new_deck2, new_shared_cards2) = flip_new_card (new_deck1, new_shared_cards1) in
    let new_table = flip_new_card (new_deck2, new_shared_cards2) in
  {st with table = new_table; street =st.street+1}

let make_turn_or_river_card st =
  let new_table = flip_new_card st.table  in
  {st with table = new_table; street =st.street+1; latest_bet=0}

(* [card_list_wo_options c_option_list] is a helper function that returns the card list
without the options*)
let card_list_wo_options c_option_list =
  match c_option_list with
  |None -> []
  |Some x -> x


 let string_of_state st =
  let state_string = "You are currently on round: " ^(string_of_int st.round) ^
  " and street " ^ (string_of_int st.street) ^ ". The pot is " ^
  (string_of_int st.pot) ^ ". And the latest bet was " ^
  (string_of_int st.latest_bet) ^ ". The current shared cards in
               the middle of the table are" in
  let shared_cards = card_list_wo_options (snd st.table) in
  let st_shared_cards= List.map string_of_card shared_cards in
  let state_string2=
    match st_shared_cards with
  |[] ->"Flop has not occured yet" ;
  |c1::c2::c3::c4::c5::[] -> (c1 ^ ", " ^ c2 ^ ", " ^ c3 ^ ", " ^ c4 ^ ", " ^ c5)
  |c1::c2::c3::c4::[] ->(c1 ^ ", " ^ c2 ^ ", " ^ c3 ^ ", " ^ c4)
  |c1::c2::c3::[] -> (c1 ^ ", " ^ c2 ^ ", " ^ c3)
  |_-> raise Illegal in
  state_string ^ state_string2

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

(*[do_bet st p m] is the state once player [p] bets amount [m] in state [st]*)
let do_raise st p m =
  failwith ""

let do' st p c=
  match c with
  | Call -> do_call st p
  | Fold ->  do_fold st p
  | Bet(x)->  do_bet st p x
  | Check -> do_check st p
  | Raise(x) -> do_raise st p x
  | Quit -> st


  let compare_cards c1 c2 =
    if (fst c1) > (fst c2) then 1
    else if (fst c2) > (fst c1) then -1
    else 0


  let sorted_ranks_list card_list =
    let sorted_cards = List.sort compare_cards card_list in
    let ranks_of_sorted = List.map (fun x -> fst x) sorted_cards in
    ranks_of_sorted


  let suits_list card_list =
    let list_of_suits = List.map (fun x -> snd x) card_list in
    list_of_suits

  let highest_rank card_list =
    let ranks_list = sorted_ranks_list card_list in
    match ranks_list with
    |r1::r2::r3::r4::r5::[] -> r5
    |_ -> raise Invalid

  let has_high_card card_list =
    let ranks_list = sorted_ranks_list card_list in
    let desc_order = List.rev ranks_list in
    Some (1, desc_order)

  let has_four_kind card_list =
    let ranks_list = sorted_ranks_list card_list in
    match ranks_list with
    |r1::r2::r3::r4::r5::[] -> if ((r1=r2) && (r2=r3) && (r3=r4)) then
        Some (8,(r1::r2::r3::r4::r5::[]))
      else if ((r2=r3) && (r3=r4) && (r4=r5)) then Some (8,(r2::r3::r4::r5::r1::[]))
      else None
    |_ -> raise Invalid

  let has_three_kind card_list =
    let ranks_list = sorted_ranks_list card_list in
    match ranks_list with
    |r1::r2::r3::r4::r5::[] -> if ((r1=r2) && (r2=r3)) then Some (4,(r1::r2::r3::r4::r4::[]))
      else if ((r2=r3) && (r3=r4)) then Some (4,(r2::r3::r4::r5::r1::[]))
      else if ((r3=r4) && (r4=r5)) then Some (4,(r3::r4::r5::r2::r1::[]))
      else None
    |_ -> raise Invalid

  let has_straight c_list =
    let ranks_list = sorted_ranks_list c_list in
    match ranks_list with
    |r1::r2::r3::r4::r5::[]-> if ((r1=r2 -1) && (r2=r3 -1) &&(r3=r4 -1)&&(r4=r5 -1)) then
        Some (5,(r5::r4::r3::r2::r1::[]))
      else if ((r1=2) && (r2=3) && (r3=4) && (r4=5) &&(r5=14)) then
        Some (5,(r4::r3::r2::r1::r5::[]))
      else None
    |_ -> None (*not sure if this is good design choice or if i should raise invalid??*)

  let has_flush c_list =
    let suit_list = suits_list c_list in
    match suit_list with
    |s1::s2::s3::s4::s5::[]-> if ((s1=s2) && (s2=s3) && (s3=s4) && (s4=s5)) then
        let ranks_list = List.rev (sorted_ranks_list c_list) in Some (6,ranks_list) else None
    |_-> None


  let has_straight_flush c_list =
    let check_if_straight = has_straight c_list in
    let check_if_flush = has_flush c_list in
    if (check_if_straight != None  && check_if_flush != None ) then
      let ranks_list = List.rev (sorted_ranks_list c_list) in Some (9,ranks_list) else None


  let has_royal_flush c_list =
    let check_if_straight = has_straight c_list in
    let check_if_flush = has_flush c_list in
    let check_highest_rank = highest_rank c_list in
    (*may need to change check_highest_rank=14*)
    if (check_if_straight != None && check_if_flush != None && check_highest_rank=14) then
      let ranks_list = List.rev (sorted_ranks_list c_list) in Some (10,ranks_list) else None

  let has_full_house c_list =
    let ranks_list = sorted_ranks_list c_list in
    match ranks_list with
    |r1::r2::r3::r4::r5::[] -> if ((r1=r2) && (r2=r3) && (r4=r5)) then
        Some (7,(r1::r2::r3::r4::r5::[]))
      else if ((r3=r4) && (r4=r5) && (r1=r2)) then
        Some (7,(r3::r4::r5::r1::r2::[]))
      else None
    |_ -> raise Invalid

  let rec num_same_rank c_list acc =
    match c_list with
    |[]-> acc
    |h::t-> if List.mem h t then num_same_rank t (acc+1)  else num_same_rank t acc

  let rec num_same_rank_highest c_list acc =
    match c_list with
    |[]-> acc
    |h::t-> if List.mem h t then num_same_rank_highest t h  else num_same_rank_highest t acc

  let has_pair c_list =
    let ranks_list = sorted_ranks_list c_list in
    if (has_three_kind c_list != None || has_four_kind c_list != None ||
        has_full_house c_list != None) then None
    else
      let num_pairs = num_same_rank ranks_list 0 in
      if num_pairs =1 then Some (2,(num_same_rank_highest ranks_list 0)::[]) else None


  let has_two_pair c_list =
    let ranks_list = sorted_ranks_list c_list in
    if (has_three_kind c_list != None || has_four_kind c_list != None ||
        has_full_house c_list != None) then None
    else
      let num_pairs = num_same_rank ranks_list 0  in
      if num_pairs =2 then Some (3,(num_same_rank_highest ranks_list 0)::[]) else None

let rec all_card_combinations k c_list =
  if k = 0 then
    [[]]
  else
    let rec all_combs_helper = function
      | []      -> []
      | h :: t -> List.map (fun x -> h :: x) (all_card_combinations (k - 1) t) :: all_combs_helper t in
    List.concat (all_combs_helper c_list)

let all_player_cards player st =
  let p_hand = player.two_cards in
  match (snd st.table) with
  |Some (c1::c2::c3::c4::c5::[]) -> all_card_combinations 5 (c1::c2::c3::c4::c5::[]@p_hand)
  |_ -> raise Invalid

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


(*returns true if p1_hand beats p2_hand.*)
let is_p1_best_hand p1_cards p2_cards =
  if (p1_cards=None || p2_cards=None ) then
    raise Invalid
  else
    let (Some p1_hand) = p1_cards in
    let (Some p2_hand)= p2_cards in
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


let compare_for_sort_hands hand1 hand2 =
  if is_p1_best_hand (highest_hand hand1) (highest_hand hand2) then 1
  else if is_p1_best_hand (highest_hand hand2) (highest_hand hand1) then -1
  else 0

let player_best_hand player st =
  let all_poss_five_cards = all_player_cards player st in
  let sorted_combs = List.sort compare_for_sort_hands (all_poss_five_cards) in
  let p_best_hand = List.nth sorted_combs 20 in
  p_best_hand

let game_best_player_hand p1 p2 st=
  let best_p1_cards = player_best_hand p1 st in
  let best_p2_cards =  player_best_hand p2 st in
  match compare_for_sort_hands (best_p1_cards) (best_p2_cards) with
  |1 -> p1
  | (-1) -> p2
  |_ -> raise Tie
