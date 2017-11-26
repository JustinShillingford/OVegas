open OUnit2
open Card

let prac_tests = [
  "inital test" >:: (fun _ -> assert_equal (4, Heart) (init_card 4 Heart));
]

let test_suite =
  "Poker test suite"  >::: List.flatten [
    prac_tests;
  ]

let _ = run_test_tt_main test_suite