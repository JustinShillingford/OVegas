exception InvalidBet
exception InvalidRaise
exception Tie
exception InvalidCommand of Command.command

(* [state] is a combination of players, a table, a counter keeping track of
 * turns and rounds of the game. *)
 type state = {players: Player.player list; play_round: int; bet_round: int; pot:int; table: Table.table; latest_bet:int;
              curr_player: Player.player; message: string;
              first_action: bool; latest_st_command: string option}

exception GameOver of string*state

(* [initial_state] is the initial value of a state in a typical game with
 * the initial value of players and the initial value of a table. *)
val initial_state : Player.player list -> Table.deck -> state

(* [string_of_state st] is the string representation of the state [st]. *)
val string_of_state : state -> string

val is_valid_command : state -> Command.command -> bool

(* [do st cmd] is the new state of the game after the current player used the
 * command [cmd]. *)
val do' : state -> Command.command -> state

val blinds : state -> state

val next_player : state -> Player.player

val new_play_round : state -> state
