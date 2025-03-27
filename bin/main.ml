open Final_project
open Final_project.Dining
open Final_project.Rating

exception Unknown_Correspondence

let rating_file = Csv.load "./data/ratings.csv"

let make_rating () =
  try
    print_endline "Which food would you like to leave a rating for?";
    let name = read_line () in
    print_endline
      "With 1 star being poor and 5 stars being excellent, how would you rate \
       this food? (1, 2, 3, 4, or 5)";
    let rating = read_int () in
    print_endline "Which dining hall did you try this food at?";
    let hall = read_line () in
    print_endline "Please leave any additional comments.";
    let comments = read_line () in
    let out =
      Csv.to_channel
        (Out_channel.open_gen
           [ Out_channel.Open_append ]
           0 "./data/ratings.csv")
    in
    let rating_obj = to_csv_entry (of_strings name rating hall comments) in
    Csv.output_record out rating_obj;
    Csv.close_out out;
    print_endline "Thank you for your review!"
  with Failure read_int ->
    print_endline
      "Sorry, the value you entered cannot be represented as an integer. \
       Please enter an integer between 1 and 5, inclusive. "

(*TODO I need to handle if the user inputs something that is out of bounds*)
(*TODO I also need to handle negative inputs*)

let retrieve_rating () =
  print_endline
    "inputs: 0) view rating for specific dish \n\ 
    1) view foods with specific rating \n\ 
    2) view foods within a range of ratings \n\ 
    3) view ratings for a specific dining hall";
  let input = read_line () in
  try
    match input with
    (*user wants specific dish*)
    | "0" ->
        print_endline
          "Please enter in the dish you would like to give a rating for";
        let dish_name = read_line () in
        let dish_ratings = retrieve_ratings_name rating_file dish_name in
        print_endline (print_ratings dish_ratings)
    (*user wants specific rating*)
    | "1" ->
        print_endline
          "Please enter in the rating number from 1-5 you would like to view";
        let rating = int_of_string (read_line ()) in
        let certain_rating = retrieve_ratings_int rating_file rating in
        print_endline (print_ratings certain_rating)
    (*user wants rating range*)
    (*TODO i need to handle the event if the user does not put in numbers that are ascending*)
    (*TODO i need to check how many arguments they put in*)
    | "2" ->
        print_endline
          "Please enter in the rating number range from 1-5 you would like to \
           view";
        let rating1 = int_of_string Sys.argv.(Array.length Sys.argv - 1) in
        let rating2 = int_of_string Sys.argv.(Array.length Sys.argv - 2) in
        let range_ratings =
          retrieve_ratings_int_range rating_file rating1 rating2
        in
        print_endline (print_ratings range_ratings)
    (*user wants specific dining hall*)
    | "3" ->
        print_endline
          "Please enter in the dining hall name you would like to view";
        let rating1 = int_of_string Sys.argv.(Array.length Sys.argv - 1) in
        let rating2 = int_of_string Sys.argv.(Array.length Sys.argv - 2) in
        let range_ratings =
          retrieve_ratings_int_range rating_file rating1 rating2
        in
        print_endline (print_ratings range_ratings)
    | _ -> print_endline "Please choose only one of the above"
  with Failure _ -> print_endline "Please put in an integer"

let handle_feedback () =
  print_endline "Would you like to submit feedback or see a rating?";
  print_endline "Input: 0 - input feedback, 1 - view rating";
  let input = read_line () in
  if input = "0" then make_rating ()
  else if input = "1" then retrieve_rating ()
  else print_endline "Please choose one of the valid options."

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
    (*0 is for feedback, 1 - 6 are the cuisine/dietary stuff, 7-9 are dining
      halls, add okenshields later*)
    print_endline
      "0) Leave/View a rating \n\
       1) Asian \n\
       2) American \n\
       3) Mexican \n\
       4) Italian \n\
       5) Vegetarian \n\
       6) Vegan \n\
       7) Morrison \n\
       8) Appel\n";
    try
      let user_input = read_line () in
      let int_input = int_of_string user_input in
      if int_input = 0 then handle_feedback ()
      else
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
    | Unknown_Index _ -> print_endline "please input only one of the choices"
    | Invalid_Csv "Invalid CSV format" -> print_endline "Invalid CSV format"
    | Sys_error _ -> print_endline "please input a number for your selection")
