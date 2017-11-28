(* [state] is a combination of players, a table, a counter keeping track of
 * turns and rounds of the game. *)
type state = {players: Player.player list; round: int; street: int; pot:int; table: Table.table; latest_bet:int}

(* [initial_state] is the initial value of a state in a typical game with
 * the initial value of players and the initial value of a table. *)
val initial_state : Player.player list -> Table.deck -> state

(* [string_of_state st] is the string representation of the state [st]. *)
   val string_of_state : state -> string

(* [do st p cmd] is the new state of the game after player [p] used the
 * command [cmd]. *)
val do' : state -> Player.player -> Command.command -> state

    (* (* [hand_cards st] is the new state where players have been given cards
 * from the deck on the table. *)
val hand_cards : state -> state   *)
