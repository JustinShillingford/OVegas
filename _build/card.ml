exception InvalidCard

type suit = Heart | Club | Diamond | Spade

type rank = int

type card = rank * suit

let rep_ok c =
  match c with
  | (r, s) -> begin
    if ((r >= 1 && r <= 13)
    && (s = Heart || s = Club || s = Diamond || s = Spade)) then c else raise InvalidCard
  end
  
let init_card (r : rank) (s : suit) =
  match (r, s) with
  | (r, s) -> rep_ok (r, s)

(* [string_of_suit s] is the string representation of the suit [s] of a card *)
let string_of_suit s =
  match s with
  | Heart -> "Hearts"
  | Club -> "Clubs"
  | Diamond -> "Diamonds"
  | Spade -> "Spades"

let string_of_card c =
  match rep_ok c with
  | (r, s) -> begin
    if ((r >= 2 && r <= 10)) then (string_of_int r) ^ " of " ^ (string_of_suit s)
    else if (r = 14) then "Ace of " ^ (string_of_suit s)
    else if (r = 11) then "Jack of " ^ (string_of_suit s)
    else if (r = 12) then "Queen of " ^ (string_of_suit s)
    else "King of " ^ (string_of_suit s)
  end
