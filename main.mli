(* [repl st str] is the repl that the game uses by looking at the
 * current state [st] and the command [str] issued by the player. *)
 (* Design Meeting Note: Implement parsing for the string command separately *)
 val repl : State.state -> string -> State.state


(* [playgame] is the initial function called to start the game. It uses the
 * function [repl_loop] for new turns and rounds. *)
val playgame : unit -> State.state

