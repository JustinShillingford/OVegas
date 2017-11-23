open Card
open Player
open Random

exception InvalidDeck
exception EmptyDeck

type deck = card list

type table = deck * card list

(* type pot = (player * int) list *)

(*type table = deck * pot * card list *)


(********* USE REP_OK IN EVERY FUNCTION ANY TIME THERE'S A DECK ***************)
(* Deck must have 52 cards
   Deck must have exactly 13 cards of each suit
   Deck must have exactly 4 cards of each rank
*)

(* [has13ofSuitHelper d hrt clb dmd spd] is a validation that a given deck [d]
 * contains exactly 13 cards of each suit. *)
let rec has13ofSuitHelper d hrt clb dmd spd =
  match d with
  | [] -> (spd = 13) && (clb = 13) && (hrt = 13) && (dmd = 13)
  | (_,s)::t -> begin
      match s with
      | Heart -> has13ofSuitHelper t (hrt + 1) clb dmd spd
      | Club -> has13ofSuitHelper t hrt (clb + 1) dmd spd
      | Diamond -> has13ofSuitHelper t hrt clb (dmd + 1) spd
      | Spade -> has13ofSuitHelper t hrt clb dmd (spd + 1)
  end

(* [has4ofRankHelper d rankArray] is a validation that a given deck [d]
 * contains exactly 4 cards of each rank. *)
let rec has4ofRankHelper d rankArray =
  match d with
  | [] -> Array.fold_left (fun acc x -> acc && (x=4)) true rankArray
  | (r,_)::t -> begin
      let i = r - 1 in
      let _ = (rankArray.(i) <- (rankArray.(i) + 1)) in
      has4ofRankHelper t rankArray
  end

let rep_ok d =
  let has52 = (List.length d = 52) in
  let has13ofSuit = has13ofSuitHelper d 0 0 0 0 in
  let has4ofRank = has4ofRankHelper d (Array.make 13 0) in
  if (has52 && has13ofSuit && has4ofRank) then d else raise InvalidDeck

let shuffle d =
  let rec shuffle_helper acc pull_from_d =
    if (List.length pull_from_d = 0) && (List.length acc = 52) then acc
    else begin
      let n = Random.int (List.length pull_from_d) in
      let chosen_card = List.nth pull_from_d n in
      let pull_from_d' = List.filter (fun x -> x <> chosen_card) pull_from_d in
      shuffle_helper (chosen_card::acc) pull_from_d'
    end in
  shuffle_helper [] (rep_ok d)

(* [suit_from_n n] is the suit representation of an int [n] *)
let suit_from_n n =
  match n with
  | 0 -> Heart
  | 1 -> Club
  | 2 -> Diamond
  | 3 -> Spade
  | _ -> raise InvalidCard

let new_deck () =
  let rec new_deck_helper acc nth_card =
    let suit_of_n = nth_card / 13 in
    let rank_of_n = nth_card mod 13 in
    if nth_card == 52 then List.rev acc
    else new_deck_helper ((rank_of_n + 1, suit_from_n suit_of_n)::acc) (nth_card + 1) in
  new_deck_helper [] 0

let rec flip_new_card (deck, cards) =
  match (rep_ok deck) with
  | h::t -> (t, h::cards)
  | [] -> raise EmptyDeck

(* Should we be outputting a new table to reflect the deck without those two cards? *)
let rec make_hand deck =
  match (rep_ok deck) with
  | c1::c2::t -> [c1; c2]
  | _ -> raise EmptyDeck
