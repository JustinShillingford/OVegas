type command = Call | Fold | Bet of int | Check | Raise of int | Quit

(* [parse str] is the Command representation of the user's input [str] *)
val parse : string -> command

(* [cmd_to_string cmd] is the String representation of a Command [cmd] *)
val cmd_to_string : command -> string