open Final_project
open Final_project.Dining
open Final_project.Rating

exception Unknown_Correspondence

let make_rating () =
  try
    print_endline "Which food would you like to leave a rating for?";
    let name = read_line () in
    print_endline
      "With 1 star being poor and 5 stars being excellent, how would you rate \
       this food? (1, 2, 3, 4, or 5)";
    let rating = read_int () in
    print_endline "Please leave any additional comments.";
    let comments = read_line () in
    let out =
      Csv.to_channel
        (Out_channel.open_gen
           [ Out_channel.Open_append ]
           0 "./data/ratings.csv")
    in
    let rating_obj = to_csv_entry (of_strings name rating comments) in
    Csv.output_record out rating_obj;
    Csv.close_out out;
    print_endline "Thank you for your review!"
  with Failure read_int ->
    print_endline
      "Sorry, the value you entered cannot be represented as an integer. \
       Please enter an integer between 1 and 5, inclusive. "

let finding_category category =
  match category with
  | 1 -> (1, "Asian")
  | 2 -> (1, "American")
  | 3 -> (1, "Mexican")
  | 4 -> (1, "Italian")
  | 5 -> (2, "Y")
  | 6 -> (3, "Y")
  | 7 -> (4, "Y")
  | 8 -> (5, "Y")
  | 9 -> (6, "Y")
  | _ -> raise Unknown_Correspondence

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "rate" then make_rating ()
  else (
    print_endline
      "Please choose what type of cuisine you would like to see, what dietary \
       restriction you would like to filter by, or which dining hall you are \
       interested in: ";
    (*1 - 6 are the cuisine/dietary stuff, 7-9 are dining halls, add okenshields
      later*)
    print_endline
      "1) Asian \n\
       2) American \n\
       3) Mexican \n\
       4) Italian \n\
       5) Vegetarian \n\
       6) Vegan \n\
       7) Morrison \n\
       8) Appel\n";
    try
      let user_input = read_line () in
      let filter_cat = finding_category (int_of_string user_input) in

      (*these are the filtered menus for each dining hall*)
      let new_morrison_menu = filter "data/morrison.csv" filter_cat in
      print_endline "Morrison Dining options";
      print_endline (user_menu new_morrison_menu);
      let new_appel_menu = filter "data/appel.csv" filter_cat in
      print_endline "North Star options";
      print_endline (user_menu new_appel_menu)
    with
    | Unknown_Correspondence ->
        print_endline "please input only one of the choices"
    | Sys_error _ -> print_endline "please input a number for your selection")
