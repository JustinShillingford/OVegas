open Card

exception InvalidDeck
exception EmptyDeck

type deck = card list

(* type pot = (player * int) list *)

type table = deck * card list option
(* type table = deck * pot * card list *)


let rep_ok d =
  failwith "Unimplemented"

let shuffle d =
  failwith "Unimplemented"

let new_deck () =
  failwith "Unimplemented"

let rec flip_new_card (deck, cards) =
  failwith "Unimplemented"

let rec two_cards deck =
  failwith "Unimplemented"
