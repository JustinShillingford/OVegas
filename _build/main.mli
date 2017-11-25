(* [repl st str] is the repl loop that the game uses by looking at the
   current state [st] and the command [str] issued by the player. 
   [p] is the player who has just inputted a command into the repl.  *)
 (* Design Meeting Note: Implement parsing for the string command separately. *)
 val repl : State.state -> Player.player -> unit


(* [playgame] is the initial function called to start the game. It uses the
 * function [repl] for new turns and rounds. *)
val playgame : unit -> State.state