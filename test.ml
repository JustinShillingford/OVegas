open OUnit2
open Card
open Command
open Player
open Table

let prac_tests = [
  "inital test" >:: (fun _ -> assert_equal (4, Heart) (init_card 4 Heart));
]

let card_tests = [
  "rep_ok" >:: (fun _ -> assert_equal (10,Club) (Card.rep_ok (10,Club)));
  "init_card" >:: (fun _ -> assert_equal (3,Diamond) (init_card 3 Diamond));
  "string_of_card" >:: (fun _ -> assert_equal "Ace of Spades" (string_of_card (14,Spade)));
]

let command_tests = [
  "call1" >:: (fun _ -> assert_equal Call (parse " Call"));
  "call2" >:: (fun _ -> assert_equal Call (parse "call "));
  "call3" >:: (fun _ -> assert_equal Call (parse "call test some more words"));
  "fold1" >:: (fun _ -> assert_equal Fold (parse "Fold "));
  "fold2" >:: (fun _ -> assert_equal Fold (parse " fold "));
  "fold3" >:: (fun _ -> assert_equal Fold (parse " fold here's some extra text"));  
  "bet1" >:: (fun _ -> assert_equal (Bet(10)) (parse "bet $10"));
  "bet2" >:: (fun _ -> assert_equal (Bet(40)) (parse " Bet 40"));  
  "quit1" >:: (fun _ -> assert_equal Quit (parse " quit "));
  "quit2" >:: (fun _ -> assert_equal Quit (parse "Quit"));
  "quit3" >:: (fun _ -> assert_equal Quit (parse "Quit hope this still quits"));  
  "check1" >:: (fun _ -> assert_equal Check (parse " check "));
  "check2" >:: (fun _ -> assert_equal Check (parse "Check"));
  "raise1" >:: (fun _ -> assert_equal (Raise(30)) (parse "raise $30 "));
  "raise2" >:: (fun _ -> assert_equal (Raise(0)) (parse "raise 0"));
]

let freshdeck = new_deck ()

let table_tests = [
  let freshdeck_lst = [(14, Heart); (2, Heart); (3, Heart); (4, Heart); (5, Heart); (6, Heart);        (7, Heart); (8, Heart); (9, Heart); (10, Heart); (11, Heart); (12, Heart);     
  (13, Heart); (14, Club); (2, Club); (3, Club); (4, Club); (5, Club);
  (6, Club); (7, Club); (8, Club); (9, Club); (10, Club); (11, Club);
  (12, Club); (13, Club); (14, Diamond); (2, Diamond); (3, Diamond);
  (4, Diamond); (5, Diamond); (6, Diamond); (7, Diamond); (8, Diamond);
  (9, Diamond); (10, Diamond); (11, Diamond); (12, Diamond); (13, Diamond);
  (14, Spade); (2, Spade); (3, Spade); (4, Spade); (5, Spade); (6, Spade);
  (7, Spade); (8, Spade); (9, Spade); (10, Spade); (11, Spade); (12, Spade);
  (13, Spade)] in 
  "rep_ok" >:: (fun _ -> assert_equal (freshdeck) (Table.rep_ok (new_deck ())));
  "newdeck" >:: (fun _ -> assert_equal (freshdeck_lst) (new_deck ()));
]


let test_suite =
  "Poker test suite"  >::: List.flatten [
    prac_tests;
    card_tests;
    command_tests;
    table_tests;
  ]

let _ = run_test_tt_main test_suite