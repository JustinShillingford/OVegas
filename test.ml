open OUnit2
open Card
open Command

let prac_tests = [
  "inital test" >:: (fun _ -> assert_equal (4, Heart) (init_card 4 Heart));
]

let card_tests = [
  "rep_ok" >:: (fun _ -> assert_equal (10,Club) (rep_ok (10,Club)));
  "init_card" >:: (fun _ -> assert_equal (3,Diamond) (init_card 3 Diamond));
  "string_of_card" >:: (fun _ -> assert_equal "Ace of Spades" (string_of_card (14,Spade)));
]

let command_tests = [
  "call1" >:: (fun _ -> assert_equal Call (parse " Call"));
  "call2" >:: (fun _ -> assert_equal Call (parse "call "));
  "fold1" >:: (fun _ -> assert_equal Fold (parse "Fold "));
  "fold2" >:: (fun _ -> assert_equal Fold (parse " fold "));
  "bet1" >:: (fun _ -> assert_equal (Bet(10)) (parse "bet 10"));
  "bet2" >:: (fun _ -> assert_equal (Bet(40)) (parse " Bet 40"));  
  "quit1" >:: (fun _ -> assert_equal Quit (parse " quit "));
  "quit2" >:: (fun _ -> assert_equal Quit (parse "Quit"));
  "check1" >:: (fun _ -> assert_equal Check (parse " check "));
  "check2" >:: (fun _ -> assert_equal Check (parse "Check"));
  "raise1" >:: (fun _ -> assert_equal (Raise(30)) (parse "raise 30 "));
  "raise2" >:: (fun _ -> assert_equal (Raise(0)) (parse "raise 0"));
]

let test_suite =
  "Poker test suite"  >::: List.flatten [
    prac_tests;
    card_tests;
    command_tests;
  ]

let _ = run_test_tt_main test_suite