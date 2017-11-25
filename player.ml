open Table
open Card

type hand = card list

type player = {id: string; two_cards: hand; money: int}

let init_player name deck =
  let (t, c1, c2) = make_hand deck in
  ({id = name; two_cards = [c1;c2] ; money=100}, fst t)

let string_of_hand h =
  failwith "Unimplemented"

(* let is_small_blind p st =
  failwith "Unimplemented"

let is_big_blind p st =
  failwith "Unimplemented"

let has_pair p t =
  failwith "Unimplemented"

let has_two_pair p t =
  failwith "Unimplemented"

let has_three_kind p t =
  failwith "Unimplemented"

let has_straight p t =
  failwith "Unimplemented"

let has_flush p t =
  failwith "Unimplemented"

let has_full_house p t =
  failwith "Unimplemented"

let has_four_kind p t =
  failwith "Unimplemented"

let has_straight_flush p t =
  failwith "Unimplemented"

let has_royal_flush p t =
  failwith "Unimplemented"
 *)
