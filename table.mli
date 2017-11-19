open Card
open Player

(* [deck] is a combination of cards where no two cards can be similar. *)
type deck

(* [pot] is an association between a player and the amount of money they
 * contributed to the total pot. *)
type pot

(* [table] is a combination of a deck, a pot, and table cards. *)
type table

(* [shuffle d] is the copy of deck [d] where the order of cards has been
 * randomly changed. *)
val shuffle : deck -> deck

(* [rep_ok d] is the boolean checking if deck [d] does not have similar cards. *)
val rep_ok : deck -> bool

(* [new_deck] is the standard 52 cards poker deck. *)
val new_deck : deck

(* [flip_new_card t] adds a new card to the table cards of the table [t]. *)
val flip_new_card : table -> table