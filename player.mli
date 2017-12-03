
(* [hand] is the type of a hand. It's a combination of two cards. *)
type hand = Card.card list

(* [player] is the type of a player. Each player has an id, a name,
 * an indicator if it's an AI, a hand, and a score *)
type player= {id: string; two_cards: hand; money: int; latest_command: string option; remaining_in_round:bool; money_in_pot:int; is_human:bool}

(* [init_player] is a new player (not an AI) and the remaining deck after
    dealing that player's hand. *)
val init_player : string -> Table.deck -> player * Table.deck


(* [string_of_hand h] is the string representation of the hand [h]. *)
val string_of_hand : hand -> string
