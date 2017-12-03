open Table
open Card

type hand = card list

type player = {id: string; two_cards: hand; money: int;
              latest_command: string option; remaining_in_round: bool;
              money_in_pot: int; is_human:bool}

let init_player name deck =
  let (t, c1, c2) = make_hand deck in
  ({id = name; two_cards = [c1;c2] ; money=100; latest_command=None;
    remaining_in_round=true; money_in_pot=0; is_human= (if name="AI" then false else true)}, fst t)

let string_of_hand h =
  (* Can also use this function to draw the ASCII representation of cards *)
  match h with
  | f::s::[] -> "This hand has a " ^ string_of_card f ^ " and a " ^ string_of_card s
  | _ -> failwith "something is seriously wrong here."