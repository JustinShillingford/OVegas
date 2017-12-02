type command = Call | Fold | Bet of int | Check | Raise of int | Quit

exception Illegal

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
    if (String.contains trimmed_str ' ') then
      let string_len = (String.length trimmed_str - (first_space + 1)) in
      String.sub trimmed_str (first_space + 1) string_len
    else
      "" in
      match String.trim (String.lowercase_ascii first_word) with
    | "call" -> Call
    | "fold" -> Fold
    | "bet" -> Bet (int_of_string second_half)
    | "quit" -> Quit
    |_-> raise Illegal
