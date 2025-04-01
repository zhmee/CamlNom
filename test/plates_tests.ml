open OUnit2
open Final_project

let small_test_menu = Dining.make_menu "../data/small_dining.csv"
let example_ratings = Csv.load "../data/test_ratings.csv" ~strip:true

let ex_drink =
  [ "Flavor Infused Water Station"; "None"; "Y"; "Y"; "Y"; "Y"; "Y" ]

let ex_main = [ "House Made Pasta"; "Italian"; "Y"; "Y"; "Y"; "Y"; "N" ]

let ex_protein =
  [ "Chicken Souvlaki"; "Mediterranean"; "N"; "N"; "Y"; "N"; "N" ]

let ex_side = [ "Greek Salad"; "Mediterranean"; "Y"; "N"; "Y"; "N"; "N" ]

let ex_dessert =
  [
    "Overnight Oats with Blueberry & Banana"; "Dessert"; "Y"; "Y"; "Y"; "N"; "N";
  ]

let id x = x
let tests = "test suite" >::: []
let _ = run_test_tt_main tests
