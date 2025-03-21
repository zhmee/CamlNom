open Final_project

let () = print_endline "Hello, World!"

let () =
  print_endline
    Dining.(string_of_menu (filter "data/small_dining.csv" (1, "Asian")))
