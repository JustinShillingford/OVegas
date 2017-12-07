type command = Call | Fold | Bet of int | Check | Raise of int | Quit

(* [parse] takes in string from user input  *)
val parse : string -> command

(*[cmd_to_string cmd] converts commands to strings *)
val cmd_to_string : command -> string