types
table record
 deck
 middle_cards
 players
 pot

type table
type deck
type pot

functions
shuffle
rep_ok
new_deck
hand_cards
flip_new_card

val shuffle : deck -> deck
val rep_ok : deck -> boolean
val new_deck : deck
val flip_new_card : table -> table