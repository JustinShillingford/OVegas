
(* [suit] is the type of a card suit. It can be heart, club, diamond,
 * or spade. *)
type suit

(* [rank] is the type of a card rank. It's any value between 1 and 13. *)
type rank

(* [card] is the type of a card. It's a combination of a rank and a suit. *)
type card

(* [rep_ok c] is the boolean which checks if a card is a proper suit
* and proper rank. *)
val rep_ok : card -> bool

(* [init_card r s] is the card which has rank [r] and suit [s]. *)
val init_card : rank -> suit -> card

(* [string_of_card c] is the string representation of the card [c]. *)
val string_of_card : card -> string

