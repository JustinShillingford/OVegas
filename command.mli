
type command = Call | Fold | Bet of int | Quit

val parse : string -> command
