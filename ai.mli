open Player

(* [new_AI] is the initial value of a new player where the boolean checking
 * if it's an AI is set on true. *)
val new_AI : player

(* [return_command h t] is the command that the AI issues after looking at
 * the cards in its hand and the cards on the table. *)
val return_command : hand -> table -> command