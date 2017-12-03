open Card
open State
open Player
open Table

exception Mismatch

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

let print_no_cards p =
  let e = "\n" in
  print_string (e ^ e ^ e);
  print_pot p;
  print_string (e ^ e ^ e)

let build_table s =
  let players = s.players in
  let round = s.bet_round in
  let table = s.table in
  let pot = s.pot in
  let human = List.hd players in
  let ai = List.nth players 1 in
  let humanCards = human.two_cards in
  let aiCards = ai.two_cards in
  let middleCards = match snd table with
    | None -> []
    | Some cards -> cards
  in
  let middleSize = List.length middleCards in

  if (round <= 1 && middleSize == 0) then begin
    print_facedown ();
    print_no_cards pot;
    print_two_cards (List.hd humanCards) (List.nth humanCards 1)
  end
  else if (round == 2 && middleSize == 3) then begin
    print_facedown ();
    print_three_cards (List.hd middleCards) (List.nth middleCards 1) (List.nth middleCards 2);
    print_pot pot;
    print_two_cards (List.hd humanCards) (List.nth humanCards 1)
  end
  else if (round == 3 && middleSize == 4) then begin
    print_facedown ();
    print_four_cards (List.hd middleCards) (List.nth middleCards 1) (List.nth middleCards 2) (List.nth middleCards 3);
    print_pot pot;
    print_two_cards (List.hd humanCards) (List.nth humanCards 1)
  end
  else if (round == 4 && middleSize == 5) then begin
    print_facedown ();
    print_five_cards (List.hd middleCards) (List.nth middleCards 1) (List.nth middleCards 2) (List.nth middleCards 3) (List.nth middleCards 4);
    print_pot pot;
    print_two_cards (List.hd humanCards) (List.nth humanCards 1)
  end
  else if (round == 5 && middleSize == 5) then begin
    print_two_cards (List.hd aiCards) (List.nth aiCards 1);
    print_five_cards (List.hd middleCards) (List.nth middleCards 1) (List.nth middleCards 2) (List.nth middleCards 3) (List.nth middleCards 4);
    print_pot pot;
    print_two_cards (List.hd humanCards) (List.nth humanCards 1)
  end
  else raise Mismatch

let win_message () =
  