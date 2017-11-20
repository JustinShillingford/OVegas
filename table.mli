open Card

(* [deck] is a combination of cards where no two cards can be similar. *)
type deck

(*(* [pot] is an association between a player and the amount of money they
 * contributed to the total pot. *)
type pot *)

(* [table] is a combination of a deck and middle cards. *)
type table

(* [shuffle d] is the copy of deck [d] where the order of cards has been
 * randomly changed. *)
val shuffle : deck -> deck

(* [rep_ok d] returns [d] if [d] satisfies the representation
   * invariants. *)
val rep_ok : deck -> deck

(* [new_deck] is the standard 52 cards poker deck. *)
val new_deck : unit -> deck

(* [flip_new_card t] adds a new card to the middle cards of the table [t]. *)
val flip_new_card : table -> table

(* [make_hand t] hands 2 cards to a player but also changes table
   such that those 2 cards are removed from the deck. *)
val make_hand : deck -> hand
