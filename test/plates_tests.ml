open OUnit2
open Final_project

let small_test_menu = Dining.make_menu "../data/small_dining.csv"
let example_ratings = Csv.load "../data/test_ratings.csv" ~strip:true

let ex_drink =
  Some [ "Flavor Infused Water Station"; "None"; "Y"; "Y"; "Y"; "Y"; "Y" ]

let ex_main = Some [ "House Made Pasta"; "Italian"; "Y"; "Y"; "Y"; "Y"; "N" ]

let ex_protein =
  Some [ "Chicken Souvlaki"; "Mediterranean"; "N"; "N"; "Y"; "N"; "N" ]

let ex_side = Some [ "Greek Salad"; "Mediterranean"; "Y"; "N"; "Y"; "N"; "N" ]

let ex_dessert =
  Some
    [
      "Overnight Oats with Blueberry & Banana";
      "Dessert";
      "Y";
      "Y";
      "Y";
      "N";
      "N";
    ]

(**[make_plate_name_test name n h c d m p s e exp] is the test with name [name]
   that the plate made using the parameter [n h c d m p s e] has the name [exp]*)
let make_plate_name_test name n h c d m p s e exp =
  name >:: fun _ ->
  assert_equal exp Plates.(name (make_plate n h c d m p s e)) ~printer:Fun.id

(**[make_plate_hall_test name n h c d m p s e exp] is the test with name [name]
   that the plate made using the parameter [n h c d m p s e] has the hall [exp]*)
let make_plate_hall_test name n h c d m p s e exp =
  name >:: fun _ ->
  assert_equal exp Plates.(hall (make_plate n h c d m p s e)) ~printer:Fun.id

(**[make_plate_comment_test name n h c d m p s e exp] is the test with name
   [name] that the plate made using the parameter [n h c d m p s e] has the
   comment [exp]*)
let make_plate_comment_test name n h c d m p s e exp =
  name >:: fun _ ->
  assert_equal exp Plates.(comment (make_plate n h c d m p s e)) ~printer:Fun.id

(**[make_plate_vegan_test name n h c d m p s e exp] is the test with name [name]
   that the plate made using the parameter [n h c d m p s e] has the vegetarian
   status [exp]*)
let make_plate_vegan_test name n h c d m p s e exp =
  name >:: fun _ ->
  assert_equal exp
    Plates.(is_vegan (make_plate n h c d m p s e))
    ~printer:string_of_bool

let id x = x

let tests =
  "test suite"
  >::: [
         make_plate_name_test "Empty name plate test" "" "Morrison"
           "No comments..." ex_drink ex_main ex_protein ex_side ex_dessert "";
         make_plate_name_test "Non-empty name plate test" "BEST PLATE EVER"
           "Morrison" "No comments..." ex_drink ex_main ex_protein ex_side
           ex_dessert "BEST PLATE EVER";
         make_plate_hall_test "Empty hall plate test" "Name" "" "No comments..."
           ex_drink ex_main ex_protein ex_side ex_dessert "";
         make_plate_hall_test "Morrison hall plate test" "Name" "Morrison"
           "No comments..." ex_drink ex_main ex_protein ex_side ex_dessert
           "Morrison";
         make_plate_comment_test "Empty comment plate test" "Name" "Morrison" ""
           ex_drink ex_main ex_protein ex_side ex_dessert "";
         make_plate_comment_test "Non-empty hall plate test" "Name" "Morrison"
           "COMMENT" ex_drink ex_main ex_protein ex_side ex_dessert "COMMENT";
         make_plate_vegan_test "Just water plate vegan test" "Vegan Water plate"
           "Morrison" "this is just a plate of water" ex_drink None None None
           None true;
         make_plate_vegan_test "Chicken and water plate non-vegan test"
           "Chicken + Water plate" "Morrison" "water + chicken" ex_drink None
           ex_protein None None false;
         make_plate_vegan_test "Empty plate vegan test" "Empty plate" "Morrison"
           "empty plate" None None None None None true;
       ]

let _ = run_test_tt_main tests
