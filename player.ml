open Table
open Card

exception Illegal
exception Invalid

type hand = card list

type player = {id: string; two_cards: hand; money: int; latest_command: string option}

let init_player name deck =
  let (t, c1, c2) = make_hand deck in
  ({id = name; two_cards = [c1;c2] ; money=100; latest_command=None}, fst t)

let string_of_hand h =
  let hand_string = List.map string_of_card h in
  match hand_string with
  |[] -> "empty hand"
  |c1::c2::[] -> c1 ^ ", " ^ c2
  |_-> raise Illegal

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

let has_four_kind card_list =
  let ranks_list = sorted_ranks_list card_list in
  match ranks_list with
  |r1::r2::r3::r4::r5::[] -> if (((r1=r2) && (r2=r3) && (r3=r4)) || ((r2=r3) && (r3=r4) && (r4=r5))) then
      true else false
  |_ -> raise Invalid

let has_three_kind card_list =
  let ranks_list = sorted_ranks_list card_list in
  match ranks_list with
  |r1::r2::r3::r4::r5::[] -> if (((r1=r2) && (r2=r3)) || ((r2=r3) && (r3=r4)) || ((r3=r4) && (r4=r5))) then
      true else false
  |_ -> raise Invalid

let has_straight c_list =
  let ranks_list = sorted_ranks_list c_list in
  match ranks_list with
  |r1::r2::r3::r4::r5::[]-> if ((r1=r2 -1) && (r2=r3 -1) && (r3=r4 -1) &&(r4=r5 -1)) then true
    else false
  |_ -> false (*not sure if this is good design choice or if i should raise invalid??*)

let has_flush c_list =
  let suit_list = suits_list c_list in
  match suit_list with
  |s1::s2::s3::s4::s5::[]-> if ((s1=s2) && (s2=s3) && (s3=s4) && (s4=s5)) then true else false
  |_-> false


let has_straight_flush c_list =
  let check_if_straight = has_straight c_list in
  let check_if_flush = has_flush c_list in
  if (check_if_straight && check_if_flush) then true else false


let has_royal_flush c_list =
  let check_if_straight = has_straight c_list in
  let check_if_flush = has_flush c_list in
  let check_highest_rank = highest_rank c_list in
  (*may need to change check_highest_rank=14*)
  if (check_if_straight && check_if_flush && check_highest_rank=14) then true else false

let has_full_house c_list =
  let ranks_list = sorted_ranks_list c_list in
  match ranks_list with
  |r1::r2::r3::r4::r5::[] -> if (((r1=r2) && (r2=r3)) && (r4=r5)|| ((r3=r4) && (r4=r5) && (r1=r2))) then
      true else false
  |_ -> raise Invalid


(*
still need to write --

   let is_small_blind p st =
   failwith "Unimplemented"

   let is_big_blind p st =
   failwith "Unimplemented"

   let has_pair p t =
   failwith "Unimplemented"

   let has_two_pair p t =
   failwith "Unimplemented"




done --

   let has_three_kind p t =
   failwith "Unimplemented"

   let has_four_kind p t =
   failwith "Unimplemented"

   let has_straight p t =
   failwith "Unimplemented"

   let has_flush p t =
   failwith "Unimplemented"

   let has_straight_flush p t =
   failwith "Unimplemented"

   let has_royal_flush p t =
   failwith "Unimplemented"

   let has_full_house p t =
   failwith "Unimplemented"


*)
