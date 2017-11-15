types
player record
 int id
 string name
 boolean isAI
 int score
 has_flush
 has_ETC

type player
type hand
type command

val init_player : player
val string_of_hand : hand -> string
val is_small_blind : player -> state -> boolean
val is_big_blind : player -> state -> boolean
val has_pair : player -> table -> boolean
val has_two_pair : player -> table -> boolean
val has_three_kind : player -> table -> boolean
val has_straight : player -> table -> boolean
val has_flush : player -> table -> boolean
val has_full_house : player -> table -> boolean
val has_four_kind : player -> table -> boolean
val has_straight_flush : player -> table -> boolean
val has_royal_flush : player -> table -> boolean
