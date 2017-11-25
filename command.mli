
type command = Call | Fold | Bet of int | Check | Quit

(* [parse] takes in string from user input  *)
val parse : string -> command
