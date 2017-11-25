
type command = Call | Fold | Bet of int | Quit

(* [parse] takes in string from user input  *)
val parse : string -> command
