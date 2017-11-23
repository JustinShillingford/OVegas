open OUnit2
open Table
open Card

let prac_tests = [
  "inital test" >:: (fun _ -> assert_equal Heart (suit_from_n 0));
]

let test_suite =
  "Poker test suite"  >::: List.flatten [
    prac_tests;
  ]

let _ = run_test_tt_main test_suite