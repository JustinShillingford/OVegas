exception InvalidBet
exception InvalidRaise
exception Tie
exception InvalidCommand of Command.command

(* [state] is a combination of players, a table, a counter keeping track of
 * turns and rounds of the game. *)
 type state = {players: Player.player list; play_round: int; bet_round: int; pot:int; table: Table.table; latest_bet:int;
              curr_player: Player.player; message: string;
              first_action: bool; latest_st_command: string option; difficulty_level: string}

exception GameOver of string*state

(* [initial_state] is the initial value of a state in a typical game with
 * the initial value of players and the initial value of a table. *)
val initial_state : Player.player list -> Table.deck -> string -> state

(* [string_of_state st] is the string representation of the state [st]. *)
val string_of_state : state -> string

(* [is_valid_command st c] is a bool stating if the given command is valid
within the current state *)
val is_valid_command : state -> Command.command -> bool

(* [do' st cmd] is the new state of the game after the current player used the
 * command [cmd]. *)
val do' : state -> Command.command -> state

(* [blinds st] returns the new state after p1 and p2 have put in their small and
  big blinds *)
val blinds : state -> state

(* [next_player st] returns the player who's not the current player *)
val next_player : state -> Player.player

(* [new_play_round st] returns the state once the play round has been initiated *)
val new_play_round : state -> state

(* [new_play_round_new_deck st] returns the state once the play round has been
  initiated. It has a new deck that it shuffles *)
val new_play_round_new_deck : state -> state
