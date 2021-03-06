type command = Call | Fold | Bet of int | Check | Raise of int | Quit

exception Illegal
exception InvalidCommand of command

let parse str =
  let trimmed_str = String.trim str in
  let first_space =
    if (String.contains trimmed_str ' ') then
      String.index trimmed_str ' '
    else
      -1  in
  let first_word  =
    if (String.contains trimmed_str ' ') then
      String.lowercase_ascii (String.sub trimmed_str 0 first_space)
    else
      trimmed_str in
  let second_half =
    if (String.contains trimmed_str '$') then
      let first_dollar = String.index trimmed_str '$' in
      let string_len = (String.length trimmed_str - (first_dollar + 1)) in
      let s = String.sub trimmed_str (first_dollar + 1) string_len in
      if s = "" then "0" else s
    else if (String.contains trimmed_str ' ') then
      let string_len = (String.length trimmed_str - (first_space + 1)) in
      String.sub trimmed_str (first_space + 1) string_len
    else
      "0" in
      match String.trim (String.lowercase_ascii first_word) with
    | "call" -> Call
    | "fold" -> Fold
    | "bet" -> Bet (try int_of_string second_half with |_ -> -1)
    | "quit" -> Quit
    | "check" -> Check
    | "raise" -> Raise (try int_of_string second_half with |_ -> -1)
    |_-> Raise (-1)

let cmd_to_string cmd =
  match cmd with
  | Call -> "Call"
  | Fold -> "Fold"
  | Bet _ -> "Bet"
  | Quit -> "Quit"
  | Check -> "Check"
  | Raise _ -> "Raise"
