open OUnit2
open Final_project

let small_test_menu = Dining.make_menu "../data/small_dining.csv"

let tests =
  "test suite" >::: [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ]

let _ = run_test_tt_main tests
