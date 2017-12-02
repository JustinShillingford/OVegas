open Card

(* [suit_to_char] is the Unicode character representation of the given suit *)
let suit_to_char = function
  | Spade -> "♠"
  | Diamond -> "♦"
  | Heart -> "♥"
  | Club -> "♣"

(* [rank_to_char r] is the character representation of each rank
 *  note: the characters are actually strings with a space character at the end
 *        to handle spacing issues *)
let rank_to_char r =
  if r=10 then (string_of_int r)
  else if r=11 then "J "
  else if r=12 then "Q "
  else if r=13 then "K "
  else if r=14 then "A "
  else (string_of_int r ^ " ")

let print_facedown () =
  let l1 = "                ┌─────────┐ ┌─────────┐\n" in
  let l2 = "                │░░░░░░░░░│ │░░░░░░░░░│\n" in
  let l3 = "                │░░░░░░░░░│ │░░░░░░░░░│\n" in
  let l4 = "                │░░░░░░░░░│ │░░░░░░░░░│\n" in
  let l5 = "                │░░░░░░░░░│ │░░░░░░░░░│\n" in
  let l6 = "                │░░░░░░░░░│ │░░░░░░░░░│\n" in
  let l7 = "                └─────────┘ └─────────┘\n" in
  print_string (l1 ^ l2 ^ l3 ^ l4 ^ l5 ^ l6 ^ l7)

let print_two_cards (r1, s1) (r2, s2) =
  let r1_str = rank_to_char r1 in
  let s1_str = suit_to_char s1 in
  let r2_str = rank_to_char r2 in
  let s2_str = suit_to_char s2 in
  let l1 = "                ┌─────────┐ ┌─────────┐\n" in
  let l2 = "                │ " ^ r1_str ^ "      | │ " ^ r2_str ^ "      |\n" in
  let l3 = "                |         | |         |\n" in
  let l4 = "                |    " ^ s1_str ^ "    | |    " ^ s2_str ^ "    |\n" in
  let l5 = "                |         | |         |\n" in
  let l6 = "                |       " ^ r1_str ^ "| |       " ^ r2_str ^ "|\n" in
  let l7 = "                └─────────┘ └─────────┘\n" in
  print_string (l1 ^ l2 ^ l3 ^ l4 ^ l5 ^ l6 ^ l7)


let print_three_cards (r1,s1) (r2,s2) (r3,s3) =
  let r1_str = rank_to_char r1 in
  let s1_str = suit_to_char s1 in
  let r2_str = rank_to_char r2 in
  let s2_str = suit_to_char s2 in
  let r3_str = rank_to_char r3 in
  let s3_str = suit_to_char s3 in
  let l1 = "┌─────────┐ ┌─────────┐ ┌─────────┐\n" in
  let l2 = "│ " ^ r1_str ^ "      | │ " ^ r2_str ^ "      | │ " ^ r3_str ^ "      |\n" in
  let l3 = "|         | |         | |         |\n" in
  let l4 = "|    " ^ s1_str ^ "    | |    " ^ s2_str ^ "    | |    " ^ s3_str ^ "    |\n" in
  let l5 = "|         | |         | |         |\n" in
  let l6 = "|       " ^ r1_str ^ "| |       " ^ r2_str ^ "| |       " ^ r3_str ^ "|\n" in
  let l7 = "└─────────┘ └─────────┘ └─────────┘\n" in
  print_string (l1 ^ l2 ^ l3 ^ l4 ^ l5 ^ l6 ^ l7)

let print_four_cards (r1,s1) (r2,s2) (r3,s3) (r4,s4) =
  let r1_str = rank_to_char r1 in
  let s1_str = suit_to_char s1 in
  let r2_str = rank_to_char r2 in
  let s2_str = suit_to_char s2 in
  let r3_str = rank_to_char r3 in
  let s3_str = suit_to_char s3 in
  let r4_str = rank_to_char r4 in
  let s4_str = suit_to_char s4 in
  let l1 = "┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐\n" in
  let l2 = "│ " ^ r1_str ^ "      | │ " ^ r2_str ^ "      | │ " ^ r3_str ^ "      | │ " ^ r4_str ^ "      |\n" in
  let l3 = "|         | |         | |         | |         |\n" in
  let l4 = "|    " ^ s1_str ^ "    | |    " ^ s2_str ^ "    | |    " ^ s3_str ^ "    | |    " ^ s4_str ^ "    |\n" in
  let l5 = "|         | |         | |         | |         |\n" in
  let l6 = "|       " ^ r1_str ^ "| |       " ^ r2_str ^ "| |       " ^ r3_str ^ "| |       " ^ r4_str ^ "|\n" in
  let l7 = "└─────────┘ └─────────┘ └─────────┘ └─────────┘\n" in
  print_string (l1 ^ l2 ^ l3 ^ l4 ^ l5 ^ l6 ^ l7)

let print_five_cards (r1,s1) (r2,s2) (r3,s3) (r4,s4) (r5,s5) =
  let r1_str = rank_to_char r1 in
  let s1_str = suit_to_char s1 in
  let r2_str = rank_to_char r2 in
  let s2_str = suit_to_char s2 in
  let r3_str = rank_to_char r3 in
  let s3_str = suit_to_char s3 in
  let r4_str = rank_to_char r4 in
  let s4_str = suit_to_char s4 in
  let r5_str = rank_to_char r5 in
  let s5_str = suit_to_char s5 in
  let l1 = "┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐\n" in
  let l2 = "│ " ^ r1_str ^ "      | │ " ^ r2_str ^ "      | │ " ^ r3_str ^ "      | │ " ^ r4_str ^ "      | │ " ^ r5_str ^ "      |\n" in
  let l3 = "|         | |         | |         | |         | |         |\n" in
  let l4 = "|    " ^ s1_str ^ "    | |    " ^ s2_str ^ "    | |    " ^ s3_str ^ "    | |    " ^ s4_str ^ "    | |    " ^ s5_str ^ "    |\n" in
  let l5 = "|         | |         | |         | |         | |         |\n" in
  let l6 = "|       " ^ r1_str ^ "| |       " ^ r2_str ^ "| |       " ^ r3_str ^ "| |       " ^ r4_str ^ "| |       " ^ r5_str ^ "|\n" in
  let l7 = "└─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘\n" in
  print_string (l1 ^ l2 ^ l3 ^ l4 ^ l5 ^ l6 ^ l7)

let print_pot p =
  print_string ("\t\t\tPot: $" ^ (string_of_int p) ^ "\n")