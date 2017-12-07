open State
open Card
open Command
open Player
open Table

(*let c1=(10,Heart)
let c2=(10,Heart)
let c3=(10,Heart)
let c4=(3,Heart)
let c5=(2,Diamond)
let c6=(2,Heart)
let c7=(2,Heart)
let cards2 = [c1;c2]
let cards5 = [c1;c2;c3;c4;c5]
let cards6 = [c1;c2;c3;c4;c5;c6]
let cards7 = [c1;c2;c3;c4;c5;c6;c7] *)

(*[card_rank_compare (r1,s1) (r2,s2)] is the comparison function between r1 and r2.*)
let card_rank_compare (r1,s1) (r2,s2) =
  Pervasives.compare r1 r2

(*[sort_by_ranking cards] is the sorted list of cards by rank.*)
let sort_by_ranking cards =
  List.sort card_rank_compare cards

(*[get_suit_value s] is the integer value of a suit*)
let get_suit_value s =
  match s with
  | Club -> 1
  | Diamond -> 2
  | Heart -> 3
  | Spade -> 4

(*[card_suit_compare (r1,s1) (r2,s2)] is the comparison function between s1 and s2.*)
let card_suit_compare (r1,s1) (r2,s2) =
  let v1 = get_suit_value s1 in
  let v2 = get_suit_value s2 in
  Pervasives.compare v1 v2

(*[sort_by_suit cards] is the sorted list of cards by suit.*)
let sort_by_suit cards =
  List.sort card_suit_compare cards

(*[get_ranks cards] is a list of the ranks in the list cards.*)
let get_ranks cards =
  let rec get_ranks_helper cards acc =
    match cards with
    | [] -> acc
    | (r1,_)::t -> get_ranks_helper t (r1::acc)
  in get_ranks_helper cards []

(*[get_suits cards] is a list of the suits in the list cards*)
let get_suits cards =
  let rec get_suits_helper cards acc =
    match cards with
    | [] -> acc
    | (_,s1)::t -> get_suits_helper t (s1::acc)
  in get_suits_helper cards []

(*[ai_has_high_card cards] checks if the AI has currently a high card in hand.*)
let ai_has_high_card cards =
  match cards with
  | (r1,s1)::(r2,s2)::t ->
    if r1=13 || r1=12 || r1=11 || r2=13 || r2=12 || r2=11 then true else false
  | _ -> false

(*[ai_has_pair cards] checks if the AI's cards and the table contain a pair*)
let ai_has_pair cards =
  let sorted_cards = sort_by_ranking cards in
  let sorted_ranks = get_ranks sorted_cards in
  let rec has_pair_helper sorted_ranks =
    match sorted_ranks with
    | r1::r2::t ->
      if r1=r2 then true else has_pair_helper (r2::t)
    | _ -> false
  in has_pair_helper sorted_ranks

(*[ai_has_two_pair cards] checks if the AI's cards and the table contains
 *a two pair.*)
let ai_has_two_pair cards =
  let sorted_cards = sort_by_ranking cards in
  let sorted_ranks = get_ranks sorted_cards in
  let rec has_two_pair_helper sorted_ranks pair_found =
    match sorted_ranks with
    | r1::r2::t ->
      if r1=r2 && pair_found then true
      else if r1=r2 then has_two_pair_helper (t) true
      else has_two_pair_helper (r2::t) pair_found
    | _ -> false
  in has_two_pair_helper sorted_ranks false

(*[ai_has_three_kind cards] checks if the AI's cards and the table contain
 *a three of a kind.*)
let ai_has_three_kind cards =
  if (List.length cards)<3 then ai_has_pair cards
  else
    let sorted_cards = sort_by_ranking cards in
    let sorted_ranks = get_ranks sorted_cards in
    let rec has_three_kind_helper sorted_ranks =
      match sorted_ranks with
      | r1::r2::r3::t ->
        if r1=r2 && r2=r3 then true else has_three_kind_helper (r2::r3::t)
      | _ -> false
    in has_three_kind_helper sorted_ranks

(*[straight sorted_ranks] checks if after sorting, the ranks contain a straight.*)
let rec straight sorted_ranks =
    match sorted_ranks with
    | r1::r2::t ->
      if r1+1=r2 then straight (r2::t) else false
    | _ -> true

(*[ai_has_straight cards] checks if the AI's cards and the table contains
 *a straight.*)
let ai_has_straight cards =
  let sorted_cards = sort_by_ranking cards in
  let sorted_ranks = get_ranks sorted_cards in
  match sorted_ranks with
  | [r1;r2;r3;r4;r5;r6] ->
    straight [r1;r2;r3;r4;r5] || straight [r2;r3;r4;r5;r6]
  | [r1;r2;r3;r4;r5;r6;r7] ->
    straight [r1;r2;r3;r4;r5] || straight [r2;r3;r4;r5;r6] || straight [r3;r4;r5;r6;r7]
  | _ -> straight sorted_ranks

(*[straight sorted_suits] checks if after sorting, the ranks contain a flush.*)
let rec flush sorted_suits =
  match sorted_suits with
  | s1::s2::t ->
    if s1=s2 then flush (s2::t) else false
  | _ -> true

(*[ai_has_flush cards] checks if the AI's cards and the table contains
 *a flush.*)
let ai_has_flush cards =
  let sorted_cards = sort_by_suit cards in
  let sorted_suits = get_suits sorted_cards in
  match sorted_suits with
  | [s1;s2;s3;s4;s5;s6] ->
    flush [s1;s2;s3;s4;s5] || flush [s2;s3;s4;s5;s6]
  | [s1;s2;s3;s4;s5;s6;s7] ->
    flush [s1;s2;s3;s4;s5] || flush [s2;s3;s4;s5;s6] || flush [s3;s4;s5;s6;s7]
  | _ -> flush sorted_suits

(*[ai_has_full house cards] checks if the AI's cards and the table contains
 *a full house.*)
let ai_has_full_house cards =
  let sorted_cards = sort_by_ranking cards in
  let sorted_ranks = get_ranks sorted_cards in
  let rec has_full_house_helper sorted_ranks pair_found three_found =
    match sorted_ranks with
    | r1::r2::r3::t ->
      if r1=r2 && r2=r3 && pair_found then true
      else if r1=r2 && r2=r3 then has_full_house_helper (t) true true
      else if r1=r2 && three_found then true
      else if r1=r2 then has_full_house_helper (r3::t) true three_found
      else has_full_house_helper (r2::r3::t) pair_found three_found
    | [r1;r2] ->
      if r1=r2 && three_found then true else false
    | _ -> false
  in has_full_house_helper sorted_ranks false false

(*[ai_has_four_kind cards] checks if the AI's cards and the table contains
 *a four of a kind.*)
let ai_has_four_kind cards =
  if (List.length cards)<4 then ai_has_three_kind cards
  else
    let sorted_cards = sort_by_ranking cards in
    let sorted_ranks = get_ranks sorted_cards in
    let rec has_four_kind_helper sorted_ranks =
      match sorted_ranks with
      | r1::r2::r3::r4::t ->
        if r1=r2 && r2=r3 && r3=r4 then true else has_four_kind_helper (r2::r3::r4::t)
      | _ -> false
    in has_four_kind_helper sorted_ranks

(*[ai_has_straight_flush cards] checks if the AI's cards and the table contains
 *a straight flush.*)
let ai_has_straight_flush cards =
  ai_has_straight cards && ai_has_flush cards

(*[royal_flush cards] checks if the flush has the required card rankings.*)
let royal_flush cards =
  let ranks = get_ranks cards in
  if not (ai_has_straight_flush cards) then false else
  match ranks with
  | [r1;r2]  ->
    if r1=10 && r2=11 || r1=11 && r2=12 || r1=12 && r2=13 || r1=13 && r2=14
    then true else false
  | [r1;r2;r3;r4;r5] ->
    if r1=10 && r2=11 && r3=12 && r4=13 && r5=14
    then true else false
  | _ -> false

(*[ai_has_royal_flush cards] checks if the AI's cards and the table contains
 *a royal flush.*)
let ai_has_royal_flush cards =
  let cards = sort_by_ranking cards in
  match cards with
  | [c1;c2;c3;c4;c5;c6] ->
    royal_flush [c1;c2;c3;c4;c5] || royal_flush [c2;c3;c4;c5;c6]
  | [c1;c2;c3;c4;c5;c6;c7] ->
    royal_flush [c1;c2;c3;c4;c5] || royal_flush [c2;c3;c4;c5;c6] || royal_flush [c3;c4;c5;c6;c7]
  | _ -> royal_flush cards

(*[rank cards] returns the integer value of the best combination currently
 *in cards.*)
let rank cards =
  if ai_has_royal_flush cards then 10
  else if ai_has_straight_flush cards then 9
  else if ai_has_four_kind cards then 8
  else if ai_has_full_house cards then 7
  else if ai_has_flush cards then 6
  else if ai_has_straight cards then 5
  else if ai_has_three_kind cards then 4
  else if ai_has_two_pair cards then 3
  else if ai_has_pair cards then 2
  else if ai_has_high_card cards then 1
  else 0

(*[option_to_list lst] returns the empty list if the option is None or the
 *list inside the option if it's Some list.*)
let option_to_list lst =
  match lst with
  | None -> []
  | Some x -> x

(*[all_card_combinations k c_list] returns all possible combinations of k elements from
 *list [c_list]
 *Referenced:
 *https://codereview.stackexchange.com/questions/40366/combinations-of-size-k-from-a-list-in-ocaml*)
let rec all_card_combinations k c_list =
  if k = 0 then
    [[]]
  else
    let rec all_combs_helper = function
      | []      -> []
      | h :: t -> List.map (fun x -> h :: x) (all_card_combinations (k - 1) t) :: all_combs_helper t in
    List.concat (all_combs_helper c_list)

(*[board_combinations_left board c_list] is the possible cards from the deck
 *that can be flipped to complete the board.*)
let rec board_combinations_left board c_list =
  let len = List.length board in
  if len=0 then all_card_combinations 5 c_list
  else if len=3 then all_card_combinations 2 c_list
  else if len=4 then all_card_combinations 1 c_list
  else []

(*[hand_strength our_cards board_cards deck] is the current strength score of
 *the player by looking at the current cards in the AI's hand an the table.*)
let hand_strength our_cards board_cards deck =
  let our_rank = rank (our_cards@board_cards) in
  let opp_cardss = all_card_combinations 2 deck in
  let (ahead,tied,behind) =
    let rec helper our_rank opp_cardss board_cards ahead tied behind =
      match opp_cardss with
      | [] -> (ahead,tied,behind)
      | opp_cards::t ->
        let opp_rank = rank (opp_cards@board_cards) in
        if (our_rank > opp_rank) then helper our_rank t board_cards (ahead+.1.0) tied behind
        else if (our_rank = opp_rank) then helper our_rank t board_cards ahead (tied+.1.0) behind
        else helper our_rank t board_cards ahead tied (behind+.1.0)
    in helper our_rank opp_cardss board_cards 1.0 1.0 1.0
  in (ahead+.(tied/.2.0))/.(ahead+.tied+.behind)

(*These types are only helpers to compute the function hand_potential*)
type three = {ahead:float;tied:float;behind:float}
type threes = {ahead_now:three;tied_now:three;behind_now:three}

(*[hand_potential our_cards board_cards deck] returns the tuple (ppot,npot)
 *where [ppot] represents the score provided the next cards on the board are
 *favorable and [npot] represents the score provided the next ones are
 *unfavorable.*)
let hand_potential our_cards board_cards deck =
  let our_rank = rank (our_cards@board_cards) in
  let opp_cardss = all_card_combinations 2 deck in
  let init_hp_total = {ahead=1.0;tied=1.0;behind=1.0} in
  let init_hp = {ahead_now=init_hp_total;tied_now=init_hp_total;behind_now=init_hp_total} in
  let (hp_total,hp) =
    let rec helper1 our_cards our_rank opp_cardss board_cards hp_total hp deck =
      match opp_cardss with
      | [] -> (hp_total,hp)
      | opp_cards::t ->
        let opp_rank = rank (opp_cards@board_cards) in
        let new_hp_total =
          if (our_rank > opp_rank) then {hp_total with ahead=(hp_total.ahead+.1.0)}
          else if (our_rank = opp_rank) then {hp_total with tied=(hp_total.tied+.1.0)}
          else {hp_total with behind=(hp_total.behind+.1.0)} in
        let index =
          if (our_rank > opp_rank) then 0
          else if (our_rank = opp_rank) then 1
          else 2 in
        let board_combinations_left = board_combinations_left board_cards deck in
        let new_hp =
          let rec helper2 our_cards opp_cards board_cards hp_total hp board_combinations_left index =
            match board_combinations_left with
            | [] -> hp
            | board_combination::tl->
              let new_board = board_cards@board_combination in
              let our_best = rank (our_cards@new_board) in
              let opp_best = rank (opp_cards@new_board) in
              let new_hp =
                if (our_best>opp_best && index=0) then {hp with ahead_now=({hp.ahead_now with ahead=(hp.ahead_now.ahead+.1.0)})}
                else if (our_best=opp_best && index=0) then {hp with ahead_now=({hp.ahead_now with tied=(hp.ahead_now.tied+.1.0)})}
                else if (our_best<opp_best && index=0) then {hp with ahead_now=({hp.ahead_now with behind=(hp.ahead_now.behind+.1.0)})}
                else if (our_best>opp_best && index=1) then {hp with tied_now=({hp.tied_now with ahead=(hp.tied_now.ahead+.1.0)})}
                else if (our_best=opp_best && index=1) then {hp with tied_now=({hp.tied_now with tied=(hp.tied_now.tied+.1.0)})}
                else if (our_best<opp_best && index=1) then {hp with tied_now=({hp.tied_now with behind=(hp.tied_now.behind+.1.0)})}
                else if (our_best>opp_best && index=2) then {hp with behind_now=({hp.behind_now with ahead=(hp.behind_now.ahead+.1.0)})}
                else if (our_best=opp_best && index=2) then {hp with behind_now=({hp.behind_now with tied=(hp.behind_now.tied+.1.0)})}
                else {hp with behind_now=({hp.behind_now with behind=(hp.behind_now.behind+.1.0)})}
              in helper2 our_cards opp_cards board_cards hp_total new_hp tl index
          in helper2 our_cards opp_cards board_cards hp_total hp board_combinations_left index
        in helper1 our_cards our_rank t board_cards new_hp_total new_hp deck
    in helper1 our_cards our_rank opp_cardss board_cards init_hp_total init_hp deck
  in
  let ppot = (hp.behind_now.ahead+.(hp.behind_now.tied/.2.0)+.(hp.tied_now.ahead)/.2.0)/.(hp_total.behind+.hp_total.tied) in
  let npot = (hp.ahead_now.behind+.(hp.tied_now.behind/.2.0)+.(hp.ahead_now.tied)/.2.0)/.(hp_total.ahead+.hp_total.tied) in
  (ppot,npot)

(*[ehs our_cards board_cards deck] implements the Poker Effective Hand Strength
  algorithm that was developed by Darse Billings, Denis Papp,
  Jonathan Schaeffer and Duane Szafron.
  Reference: https://en.wikipedia.org/wiki/Poker_Effective_Hand_Strength_(EHS)_algorithm*)
let ehs our_cards board_cards deck =
  let hs = hand_strength our_cards board_cards deck in
  let (ppot,npot) = hand_potential our_cards board_cards deck in
  (hs*.(1.0-.npot))+.((1.0-.hs)*.ppot)

(*[easy_ai st] is the command of an easy AI which folds 50% of the time.*)
let easy_ai st =
  let rand = Random.int 10 in
  (*Folds 50% of the time*)
  if rand < 5 then Fold
    else if (is_valid_command st Call) then Call else Check

(*[medium_ai] is the command of a medium AI which does the next best
 *moderate move.*)
let medium_ai st =
  if (is_valid_command st Call) then Call else Check

(*[possible_bet p x] checks if the amount [x] is a valid bet for the
 *player [p].*)
let possible_bet p x =
  if x<0 then false
  else if x<=p.money then true else false

(*[possible_raise p x] checks if the amount [x] is a valid raise amount for the
 *player [p].*)
let possible_raise p x =
  if x<0 then false
  else if x<=(p.money) then true else false

(*[possible_raise2 st x] checks if the amount [x] is valid to raise.*)
let possible_raise2 st x =
  ((next_player st).money_in_pot-st.curr_player.money_in_pot)<x

(*[make_conservative_move st] is a helper function to the hard AI, if the AI
 *has a weak hand, then if folds 30% of the time.*)
let make_conservative_move st =
  let rand = Random.int 10 in
  (*Folds 30% of the time*)
  if rand < 3 then Fold
    else if (is_valid_command st Call) then Call else Check

(*[make_aggressive_move st p] is a helper function to the hard AI, which makes
 *it raise or bet when it has a good hand.*)
let make_aggressive_move st p =
  if ((is_valid_command st (Bet 50)) && (possible_bet p 50)) then (Bet 50)
  else if ((is_valid_command st (Raise 50)) && (possible_raise p 50) && (possible_raise2 st 50)) then (Raise 50)
  else medium_ai st

(*[sub_list l] is a partial list of the list [l].
 *Reference: Stack overflow: https://stackoverflow.com/questions/2710233/
 *how-to-get-a-sub-list-from-a-list-in-ocaml*)
let sub_list l =
  let rec sublist b e l =
    match l with
      [] -> failwith "sublist"
    | h :: t ->
       let tail = if e=0 then [] else sublist (b-1) (e-1) t in
       if b>0 then tail else h :: tail
  in sublist 0 ((List.length l)/10) l

(*[hard_ai] is the command of a hard AI which uses the Effective Hand Strength
 *algorithm to determine the next best move.*)
let hard_ai st =
   match st.players with
  | [p1;p2] ->
    let ai_cards = p2.two_cards in
    let table = st.table in
    let table_cards = option_to_list (snd table) in
    let deck = sub_list (fst table) in
    let ehs = ehs ai_cards table_cards deck in
    if ehs < 0.16 then (make_conservative_move st)
    else if ehs > 0.66 then (make_aggressive_move st p2)
    else medium_ai st
  | _ -> Fold

let ai_command st =
  let diff = st.difficulty_level in
  if diff="easy" then easy_ai st
  else if diff="hard" then hard_ai st
  else medium_ai st