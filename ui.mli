(* [print_facedown] is the ASCII representation of two facedown cards *)
val print_facedown : unit -> unit

(* [print_two_cards (r1,s1) (r2,s2)] is the ASCII representation of two faceup
 *  cards *)
val print_two_cards : Card.card -> Card.card -> unit

(* [print_three_cards (r1,s1) (r2,s2) (r3,s3)] is the ASCII representation of
 *  three faceup cards *)
val print_three_cards : Card.card -> Card.card -> Card.card -> unit

(* [print_four_cards (r1,s1) (r2,s2) (r3,s3) (r4,s4)] is the ASCII representation
 *  of four faceup cards *)
val print_four_cards : Card.card -> Card.card -> Card.card -> Card.card -> unit

(* [print_five_cards (r1,s1) (r2,s2) (r3,s3) (r4,s4) (r5,s5)] is the ASCII
 *  representation of five faceup cards *)
val print_five_cards : Card.card -> Card.card -> Card.card -> Card.card -> Card.card -> unit

(* [print_pot p] is the printed representation of the pot *)
val print_pot : int -> unit

(* [print_no_cards p] is the ASCII representation of empty space with the pot
 *  in the center *)
val print_no_cards : int -> unit

(* [build_table plyrs pot tbl] is the ASCII representation of the table *)
val build_table : State.state -> unit

(* [win_message] is the ASCII win message for the player *)
val win_message : unit -> unit

(* [lose_message] is the ASCII lose message for the player *)
val lose_message : unit -> unit
