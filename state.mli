types
state record
 players
 turn/round
 table

type state

functions
initial_state
print_state
do(state,command,player)
is_small_blind(state,player)
is_big_blind(state,player)

val initial_state : state
val string_of_state : state -> string
val do : state -> command -> player -> state
val hand_cards : state -> state