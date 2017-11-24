open State
open Player

let rec repl st str =
  failwith "Unimplemented"

 let playgame () =
  ANSITerminal.(print_string [red] "\nWelcome to OVegas, the OCaml Texas Hold'em!");
  print_endline "Please enter the name of the first player.";
  print_string "> ";
  let player1_name = begin
    match read_line () with
    | exception End_of_file -> ""
    | player1_name -> player1_name
  end in
  print_endline "Please enter the name of the secon player.";
  print_string "> ";
  let player2_name = begin
    match read_line () with
    | exception End_of_file -> ""
    | player2_name -> player2_name
  end in
  (* The part above should allow for the program to obtain the names of the 2
   * players. Need to find way to use these names in st for the repl. *)


(* let () = playgame () *)