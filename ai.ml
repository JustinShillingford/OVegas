(*hand , table -> command*)
(*hand ==> card list*)
(*table ==> card list*)
(*decide with 0/3/4/5 cards*)
(*decide with 2/5/6/7 cards*)
open State
open Card
open Command
open Player

(* type suit = Heart | Club | Diamond | Spade

type rank = int

type card = rank * suit

let c1=(10,Heart)
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


let card_rank_compare (r1,s1) (r2,s2) =
  Pervasives.compare r1 r2

let sort_by_ranking cards =
  List.sort card_rank_compare cards

let get_suit_value s =
  match s with
  | Club -> 1
  | Diamond -> 2
  | Heart -> 3
  | Spade -> 4

let card_suit_compare (r1,s1) (r2,s2) =
  let v1 = get_suit_value s1 in
  let v2 = get_suit_value s2 in
  Pervasives.compare v1 v2

let sort_by_suit cards =
  List.sort card_suit_compare cards

let get_ranks cards =
  let rec get_ranks_helper cards acc =
    match cards with
    | [] -> acc
    | (r1,_)::t -> get_ranks_helper t (r1::acc)
  in get_ranks_helper cards []

let get_suits cards =
  let rec get_suits_helper cards acc =
    match cards with
    | [] -> acc
    | (_,s1)::t -> get_suits_helper t (s1::acc)
  in get_suits_helper cards []

let ai_has_high_card cards =
  match cards with
  | (r1,s1)::(r2,s2)::t ->
    if r1=13 || r1=12 || r1=11 || r2=13 || r2=12 || r2=11 then true else false
  | _ -> false

let ai_has_pair cards =
  let sorted_cards = sort_by_ranking cards in
  let sorted_ranks = get_ranks sorted_cards in
  let rec has_pair_helper sorted_ranks =
    match sorted_ranks with
    | r1::r2::t ->
      if r1=r2 then true else has_pair_helper (r2::t)
    | _ -> false
  in has_pair_helper sorted_ranks

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

let rec straight sorted_ranks =
    match sorted_ranks with
    | r1::r2::t ->
      if r1+1=r2 then straight (r2::t) else false
    | _ -> true

let ai_has_straight cards =
  let sorted_cards = sort_by_ranking cards in
  let sorted_ranks = get_ranks sorted_cards in
  match sorted_ranks with
  | [r1;r2;r3;r4;r5;r6] ->
    straight [r1;r2;r3;r4;r5] || straight [r2;r3;r4;r5;r6]
  | [r1;r2;r3;r4;r5;r6;r7] ->
    straight [r1;r2;r3;r4;r5] || straight [r2;r3;r4;r5;r6] || straight [r3;r4;r5;r6;r7]
  | _ -> straight sorted_ranks

let rec flush sorted_suits =
  match sorted_suits with
  | s1::s2::t ->
    if s1=s2 then flush (s2::t) else false
  | _ -> true

let ai_has_flush cards =
  let sorted_cards = sort_by_suit cards in
  let sorted_suits = get_suits sorted_cards in
  match sorted_suits with
  | [s1;s2;s3;s4;s5;s6] ->
    flush [s1;s2;s3;s4;s5] || flush [s2;s3;s4;s5;s6]
  | [s1;s2;s3;s4;s5;s6;s7] ->
    flush [s1;s2;s3;s4;s5] || flush [s2;s3;s4;s5;s6] || flush [s3;s4;s5;s6;s7]
  | _ -> flush sorted_suits

let ai_has_straight_flush cards =
  ai_has_straight cards && ai_has_flush cards

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

let ai_has_royal_flush cards =
  let cards = sort_by_ranking cards in
  match cards with
  | [c1;c2;c3;c4;c5;c6] ->
    royal_flush [c1;c2;c3;c4;c5] || royal_flush [c2;c3;c4;c5;c6]
  | [c1;c2;c3;c4;c5;c6;c7] ->
    royal_flush [c1;c2;c3;c4;c5] || royal_flush [c2;c3;c4;c5;c6] || royal_flush [c3;c4;c5;c6;c7]
  | _ -> royal_flush cards

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

let cards_ranking cards =
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

let get_cards_value cards =
  let len = List.length cards in
  let value = cards_ranking cards in
  len*value

let option_to_list lst =
  match lst with
  | None -> []
  | Some x -> x

(*let rec ai_command st =
   match st.players with
  | [p1;p2] ->
    let ai_cards = p2.two_cards in
    let table = st.table in
    let table_cards = option_to_list (snd table) in
    let cards = ai_cards@table_cards in
    let cards_value = get_cards_value cards in
    if cards_value > 0 then Call else Fold
  | _ -> Fold *)

let rec ai_command st =
  if (is_valid_command st Call) then Call else Check


let is_bluff opponent_hand table_cards =
  let cards = opponent_hand@table_cards in
  if cards_ranking cards < 2 then true else false

type opponent_data = {bluff: bool; bet: int; hand: card list; table: card list}
type ai_data = {bluff: bool; opponent_fold: bool}

(*
opponent_data:
did opponent bluff? - opponent bet amount - opponent hand - table cards

ai_data:
did ai bluff? - did opponent fold?
*)

(*
is opponent bluffing right now?

should we bluff right now?
*)