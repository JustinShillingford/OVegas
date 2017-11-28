open Table
open Card

exception Illegal

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
