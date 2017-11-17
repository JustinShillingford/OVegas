open State

(* [playgame] is the initial function called to start the game. It uses the
 * function [repl_loop] for new turns and rounds. *)
val playgame : unit -> state

(* [repl_loop st str] is the repl loop that the game uses by looking at the
 * current state [st] and the command [str] issued by the player. *)
 (* Design Meeting Note: Implement parsing for the string command separately *)
val repl_loop : state -> string -> state
