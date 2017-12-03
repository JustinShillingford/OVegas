(* [state] is a combination of players, a table, a counter keeping track of
 * turns and rounds of the game. *)
 type state = {players: Player.player list; play_round: int; bet_round: int; pot:int; table: Table.table; latest_bet:int; curr_player: Player.player; message: string}
 
(* [initial_state] is the initial value of a state in a typical game with
 * the initial value of players and the initial value of a table. *)
val initial_state : Player.player list -> Table.deck -> state

(* [string_of_state st] is the string representation of the state [st]. *)
val string_of_state : state -> string

val is_human : state -> bool

val is_valid_command : Command.command -> bool

(* [do st cmd] is the new state of the game after the current player used the
 * command [cmd]. *)
val do' : state -> Command.command -> state
