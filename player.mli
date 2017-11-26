
(* [hand] is the type of a hand. It's a combination of two cards. *)
type hand

(* [player] is the type of a player. Each player has an id, a name,
 * an indicator if it's an AI, a hand, and a score *)
type player

(* [init_player] is a new player (not an AI) and the remaining deck after
    dealing that player's hand. *)
val init_player : string -> Table.deck -> player * Table.deck

(* [string_of_hand h] is the string representation of the hand [h]. *)
val string_of_hand : hand -> string

(* (* [is_small_blind p st] is the boolean checking if the player [p] is a small
  blind in stateyeah [st]. *)
val is_small_blind : player -> State.state -> boolean

(* [is_big_blind p st] is the boolean checking if the player [p] is a big
 * blind in state [st]*)
val is_big_blind : player -> State.state -> boolean

(* [has_pair p t] is the boolean checking if the player [p] has a pair
 * by looking at table [t]. *)
val has_pair : player -> Table.table -> boolean

(* [has_two_pair p t] is the boolean checking if the player [p] has a two
 * pair by looking at table [t]. *)
val has_two_pair : player -> Table.table -> boolean

(* [has_three_kind p t] is the boolean checking if the player [p] has a
 * three of a kind by looking at table [t]. *)
val has_three_kind : player -> Table.table -> boolean

(* [has_straight p t] is the boolean checking if the player [p] has a straight
 * by looking at table [t]. *)
val has_straight : player -> Table.table -> boolean

(* [has_flush p t] is the boolean checking if the player [p] has a flush by
 * looking at table [t]. *)
val has_flush : player -> Table.table -> boolean

(* [has_full_house p t] is the boolean checking if the player [p] has a full
 * house by looking at table [t]. *)
val has_full_house : player -> Table.table -> boolean

(* [has_four_kind p t] is the boolean checking if the player [p] has a four
 * of a kind by looking at table [t]. *)
val has_four_kind : player -> Table.table -> boolean

(* [has_straight_flush p t] is the boolean checking if the player [p] has a
 * straight flush by looking at table [t]. *)
val has_straight_flush : player -> Table.table -> boolean

(* [has_royal_flush p t] is the boolean checking if the player [p] has a royal
 * flush by looking at table [t]. *)
val has_royal_flush : player -> Table.table -> boolean *)
