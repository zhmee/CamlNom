open OUnit2
open Final_project
open Final_project.Dining

let small_test_menu = Dining.make_menu "../data/small_dining.csv"

(**Test each option 
1
2
3
4
5
6
Not found
Incorrectly formatted csv file 
String_of_list test, seeing if the correct string is returned. *)


(**[list_string_conversion list] takes in a string list and combines each
   element into a singular string*)


  let make_menu_test test_name dining_hall filter_item expected =
    "Test for " ^ test_name >:: fun _ ->
    let filtered_menu = filter dining_hall filter_item in 
    let string = string_of_menu filtered_menu in 
    assert_equal expected string ~printer:Fun.id



let tests =
  "test suite" >::: [ 
      let expected_string = 
      "AppleOtherYYYYYAppleFruitYYNYYApple2YYNYYApple1YYNYY" in 
      make_menu_test "apple test" "../data/small_dining.csv" (0,"Apple") expected_string;
  ]

let _ = run_test_tt_main tests
