open Table
open Card

type hand = card list

type player = {id: string; two_cards: hand; money: int}

let init_player name deck =
  let (t, c1, c2) = make_hand deck in
  ({id = name; two_cards = [c1;c2] ; money=100}, fst t)

let string_of_hand h =
  (* Can also use this function to draw the ASCII representation of cards *)
  match h with
  | f::s::[] -> "This hand has a " ^ string_of_card f ^ " and a " ^ string_of_card s
  | _ -> failwith "something is seriously wrong here."