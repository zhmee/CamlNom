open OUnit2
open Final_project

let small_test_menu = Dining.make_menu "../data/small_dining.csv"
let id x = x

(**[filter_name_test c p e] is the test that [filter c p] produces [e]*)
let filter_name_test csv params exp =
  "filter name ---------" ^ "--------- test" >:: fun _ ->
  assert_equal exp Dining.(string_of_menu (filter csv params)) ~printer:id

let tests =
  "test suite"
  >::: [
         ("a trivial test" >:: fun _ -> assert_equal 0 0);
         filter_name_test "../data/small_dining.csv" (0, "Apple")
           "AppleOtherYYYYYAppleFruitYYNYYApple2YYNYYApple1YYNYY";
       ]

let _ = run_test_tt_main tests
