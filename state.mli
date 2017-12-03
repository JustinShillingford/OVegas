(* [state] is a combination of players, a table, a counter keeping track of
 * turns and rounds of the game. *)
type state = {players: Player.player list; play_round: int; bet_round: int; pot:int; table: Table.table; latest_bet:int; curr_player: Player.player; message: string; first_action: bool; latest_st_command: Command.command option}

(* [initial_state] is the initial value of a state in a typical game with
 * the initial value of players and the initial value of a table. *)
val initial_state : Player.player list -> Table.deck -> state

(* [string_of_state st] is the string representation of the state [st]. *)
val string_of_state : state -> string

(* [do st p cmd] is the new state of the game after player [p] used the
 * command [cmd]. *)
val do' : state -> Command.command -> state

(*[game_best_player_hand p1 p2 st] is player with the best hand at the state [st]*)
val game_best_player_hand :  Player.player -> Player.player -> state -> Player.player


val is_valid_command :  state -> Command.command -> bool 
