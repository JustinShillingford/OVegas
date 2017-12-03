open Table
open Card

exception Illegal

type hand = card list

type player = {id: string; two_cards: hand; money: int; latest_command: string option; remaining_in_round: bool; money_in_pot: int; is_human:bool}

let init_player name deck =
  let (t, c1, c2) = make_hand deck in
  ({id = name; two_cards = [c1;c2] ; money=100; latest_command=None; remaining_in_round=true; money_in_pot=0; is_human= true}, fst t)

let string_of_hand h =
  let hand_string = List.map string_of_card h in
  match hand_string with
  |[] -> "empty hand"
  |c1::c2::[] -> c1 ^ ", " ^ c2
  |_-> raise Illegal
