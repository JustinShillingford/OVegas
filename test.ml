open OUnit2
open Card
open Command
open Player
open Table
open State

let prac_tests = [
  "inital test" >:: (fun _ -> assert_equal (4, Heart) (init_card 4 Heart));
]

let card_tests = [
  "rep_ok" >:: (fun _ -> assert_equal (10,Club) (Card.rep_ok (10,Club)));
  "ace_rep_ok" >:: (fun _ -> assert_equal (14,Club) (Card.rep_ok (14,Club)));
  "init_card" >:: (fun _ -> assert_equal (3,Diamond) (init_card 3 Diamond));
  "string_of_card" >:: (fun _ -> assert_equal "10 of Spades" (string_of_card (10,Spade)));
  "string_of_card_ace" >:: (fun _ -> assert_equal "Ace of Spades" (string_of_card (14,Spade)));
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
  "bet3" >:: (fun _ -> assert_equal (Bet(-1)) (parse "bet $888 888 7"));
  "bet4" >:: (fun _ -> assert_equal (Bet(-1)) (parse " Bet 40 mmm"));
  "bet5" >:: (fun _ -> assert_equal (Bet(-1)) (parse " Bet mmm"));
  "quit1" >:: (fun _ -> assert_equal Quit (parse " quit "));
  "quit2" >:: (fun _ -> assert_equal Quit (parse "Quit"));
  "quit3" >:: (fun _ -> assert_equal Quit (parse "Quit hope this still quits"));
  "check1" >:: (fun _ -> assert_equal Check (parse " check "));
  "check2" >:: (fun _ -> assert_equal Check (parse "Check"));
  "raise1" >:: (fun _ -> assert_equal (Raise(30)) (parse "raise $30 "));
  "raise2" >:: (fun _ -> assert_equal (Raise(0)) (parse "raise 0"));
  "raise3" >:: (fun _ -> assert_equal (Raise(-1)) (parse "raise $3pp0 "));
  "raise4" >:: (fun _ -> assert_equal (Raise(-1)) (parse "raise 0ll"));
]


let freshdeck = new_deck ()

let p1_two_cards = [(14, Heart); (2, Heart)]
let p2_two_cards =[(13, Club); (5, Heart)]
let stringhand = "This hand has a Ace of Hearts and a 2 of Hearts"
let stringhand2 = "This hand has a King of Clubs and a 5 of Hearts"
let player_tests = [
  "p1hand" >:: (fun _ -> assert_equal stringhand (string_of_hand p1_two_cards));
  "p2hand" >:: (fun _ -> assert_equal stringhand2 (string_of_hand p2_two_cards));
  "init_money" >:: (fun _ -> assert_equal 500 ((fst (init_player "mishcat"
                                                       freshdeck)).money));
  "init_player" >:: (fun _ -> assert_equal "mishcat" ((fst (init_player "mishcat"
                                                              freshdeck)).id));
  "latest_st_command" >::
  (fun _ -> assert_equal None ((fst (init_player "mishcat" freshdeck)).latest_command));
  "init_money_in_pot" >::
  (fun _ -> assert_equal 0 ((fst (init_player "mishcat" freshdeck)).money_in_pot));
  "human" >::
  (fun _ -> assert_equal true ((fst (init_player "mishcat" freshdeck)).is_human));
  "ai_not_human" >::
  (fun _ -> assert_equal false ((fst (init_player "AI" freshdeck)).is_human));
  "init_player" >::
  (fun _ -> assert_equal true ((fst (init_player "ai" freshdeck)).is_human));
]

let table_tests = [
  let freshdeck_lst = [(14, Heart); (2, Heart); (3, Heart); (4, Heart); (5, Heart);
  (6, Heart); (7, Heart); (8, Heart); (9, Heart); (10, Heart); (11, Heart);
  (12, Heart); (13, Heart); (14, Club); (2, Club); (3, Club); (4, Club); (5, Club);
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

let p1 = init_player "TestPlayer1" (new_deck ())
let p2 = init_player "AI" (snd p1)
let st = initial_state [fst p1;fst p2] (snd p2) "medium"
let new_st =
  {players = [{id = "TestPlayer1"; two_cards = [(14, Heart); (2, Heart)];
              money = 490; latest_command = None; remaining_in_round = true;
              money_in_pot = 10; is_human = true};
              {id = "AI"; two_cards = [(3, Heart); (4, Heart)]; money = 480;
              latest_command = None; remaining_in_round = true;
              money_in_pot = 20; is_human = false}];
   play_round = 0;
   bet_round = 1;
   pot = 30;
   table =
    ([(5, Heart); (6, Heart); (7, Heart); (8, Heart); (9, Heart); (10, Heart);
      (11, Heart); (12, Heart); (13, Heart); (14, Club); (2, Club); (3, Club);
      (4, Club); (5, Club); (6, Club); (7, Club); (8, Club); (9, Club);
      (10, Club); (11, Club); (12, Club); (13, Club); (14, Diamond);
      (2, Diamond); (3, Diamond); (4, Diamond); (5, Diamond); (6, Diamond);
      (7, Diamond); (8, Diamond); (9, Diamond); (10, Diamond); (11, Diamond);
      (12, Diamond); (13, Diamond); (14, Spade); (2, Spade); (3, Spade);
      (4, Spade); (5, Spade); (6, Spade); (7, Spade); (8, Spade); (9, Spade);
      (10, Spade); (11, Spade); (12, Spade); (13, Spade)],
     None);
   latest_bet = 20;
   curr_player =
    {id = "TestPlayer1"; two_cards = [(14, Heart); (2, Heart)]; money = 490;
     latest_command = None; remaining_in_round = true; money_in_pot = 10;
     is_human = true};
   message = ""; first_action = true; latest_st_command = None;
   difficulty_level = "medium"}
let st_str = "You are currently on round: 0 and bet round 0. The pot is 30.
The current shared cards in the middle of the table are none since the flop
has not yet occured."

let state_tests = [
  "nextplayer" >:: (fun _ -> assert_equal (fst p2) (next_player st));
   "blinds" >:: (fun _ -> assert_equal new_st (blinds st));
  "validCommand_Call" >:: (fun _ -> assert_equal false (is_valid_command st Call));
  "validCommand_Check_pre_flop" >::
  (fun _ -> assert_equal false (is_valid_command {st with bet_round=1} Check));
  "no_call_if_fst_action" >::
  (fun _ -> assert_equal false (is_valid_command {st with bet_round=2} Call));
  "check_after_check" >::
  (fun _ -> assert_equal true
      (is_valid_command {st with bet_round=2; latest_st_command= Some "check"} Check));
  "check_after_not_check" >::
  (fun _ -> assert_equal true
      (is_valid_command {st with bet_round=2; latest_st_command= Some "call"} Check));
  "check_after_not_check" >::
  (fun _ -> assert_equal false
      (is_valid_command
         {st with bet_round=2; first_action=false; latest_st_command= Some "call"} Check));
  "validCommand_Fold" >::
  (fun _ -> assert_equal true (is_valid_command st Fold));
  "validCommand_Bet10" >::
  (fun _ -> assert_equal true (is_valid_command st (Bet(10))));
  "validCommand_Check" >::
  (fun _ -> assert_equal true (is_valid_command st Check));
  "validCommand_Raise10" >::
  (fun _ -> assert_equal false (is_valid_command st (Raise(10))));
  "validCommand_Quit" >:: (fun _ -> assert_equal true (is_valid_command st Quit));

]

let test_suite =
  "Poker test suite"  >::: List.flatten [
    prac_tests;
    card_tests;
    command_tests;
    table_tests;
    state_tests;
    player_tests
  ]

let _ = run_test_tt_main test_suite
