open Final_project
open Final_project.Dining
open Final_project.Rating

exception Unknown_Correspondence

exception Illegal_rating of int
(**[Illegal_rating rating] documents the event that the user inputs a rating
   that is deemed illegal by the specifications in [rating.mli]*)

let rating_file = Csv.load "./data/ratings.csv"
let morrison = make_menu "./data/morrison.csv"
let appel = make_menu "./data/appel.csv"

(*please dont touch these*)

(*This is the total filters that the user can filter by*)
let user_filters = Options.get_all_filters

(**[remove_item] takes in some category to be removed from the total possible
   filtering options.*)
let remove_item filter =
  user_filters := List.filter (fun x -> x <> filter) !user_filters

(**[make_rating] prompts the user for what food they would like to rate, what
   rating they would like to give, and which dining hall they had it at. Also
   writes to a csv with ratings for each feedback given.*)
let make_rating () =
  try
    print_endline "Which food would you like to leave a rating for?";
    let name = read_line () in
    print_endline
      "With 1 star being poor and 5 stars being excellent, how would you rate \
       this food? (1, 2, 3, 4, or 5)";
    let rating = read_int () in
    if rating > 5 || rating < 0 then raise (Illegal_rating rating)
    else (
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
      print_endline "Thank you for your review!")
  with
  | Illegal_rating rating ->
      Printf.printf
        "Sorry, %d is not a valid input. Please enter a value between 0 and 5.\n"
        rating
  | Failure read_int ->
      print_endline
        "Sorry, the value you entered cannot be represented as an integer. \
         Please enter an integer between 1 and 5, inclusive. "

(**[retrieve_rating] takes care of retrieving the specific category of rating
   the user wants.*)
let rec retrieve_rating () =
  print_endline
    "inputs: 0) view rating for specific dish \n\
    \ \n\
    \    1) view foods with specific rating \n\
    \ \n\
    \    2) view foods within a range of ratings \n\
    \ \n\
    \    3) view ratings for a specific dining hall";
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
        let number = read_line () in
        let rating = int_of_string number in
        let certain_rating = retrieve_ratings_int rating_file rating in
        print_endline (print_ratings certain_rating)
    (*user wants rating range*)
    | "2" ->
        print_endline
          "You have choose to view reviews that fall between certain ratings: \n\
          \          Please indicate the lower range of reviews (1-5):";
        let rating1 = read_line () in
        print_endline " Please indicate the upper range of reviews (1-5):";
        let rating2 = read_line () in
        let range_ratings =
          retrieve_ratings_int_range rating_file (int_of_string rating1)
            (int_of_string rating2)
        in
        print_endline (print_ratings range_ratings)
    (*user wants specific dining hall*)
    | "3" ->
        print_endline
          "Please enter in the dining hall name you would like to view";
        let hall = read_line () in
        let hall_ratings = retrieve_ratings_hall rating_file hall in
        print_endline (print_ratings hall_ratings)
    | _ ->
        print_endline "Please choose only one of the above";
        retrieve_rating ()
  with Failure _ ->
    print_endline "Please put in an integer";
    retrieve_rating ()

(**[handle_feedback] handles the different cases of giving or receiving feedback
   that the user may want. *)
let rec handle_feedback () =
  print_endline "Would you like to submit feedback or see a rating?";
  print_endline "Input: 0 - input feedback, 1 - view rating";
  let input = read_line () in
  if input = "0" then make_rating ()
  else if input = "1" then retrieve_rating ()
  else print_endline "Please choose one of the valid options."

(**[finding_category] finds the specific corresponding category for the user's
   choice in the driver and acts accordingly.*)
let finding_category category =
  match category with
  | 1 -> (1, "Asian")
  | 2 -> (1, "American")
  | 3 -> (1, "Mexican")
  | 4 -> (1, "Italian")
  | 5 -> (2, "Y")
  | 6 -> (3, "Y")
  (*TODO this is Morrison and Appel*)
  | 7 -> (4, "Y")
  | 8 -> (5, "Y")
  (*TODO this is okenshields*)
  | 9 -> (6, "Y")
  | _ -> raise Unknown_Correspondence

(**[finding_day] takes in the specific day's menu that the user wants and has
   the rest of the program parse accordingly.*)
let finding_day day =
  match day with
  | "1" -> "./data/Mon.csv"
  | "2" -> "./data/Tues.csv"
  | "3" -> "./data/Wed.csv"
  | "4" -> "./data/Thurs.csv"
  | "5" -> "./data/Fri.csv"
  | "6" -> "./data/Sat.csv"
  | "7" -> "./data/Sun.csv"
  | _ -> raise Unknown_Correspondence

let rec filter_multiple list =
  print_endline "Please choose what day you would like to see the menu for: "

let valid_choices input =
  match input with
  | 1 -> begin
      remove_item "Asian";
      remove_item "American";
      remove_item "Mexican";
      remove_item "Italian"
    end
  | 2 -> begin
      remove_item "Asian";
      remove_item "American";
      remove_item "Mexican";
      remove_item "Italian"
    end
  | 3 -> begin
      remove_item "Asian";
      remove_item "American";
      remove_item "Mexican";
      remove_item "Italian"
    end
  | 4 -> begin
      remove_item "Asian";
      remove_item "American";
      remove_item "Mexican";
      remove_item "Italian"
    end
  | 5 -> begin
      remove_item "Vegetarian";
      remove_item "Vegan"
    end
  | 6 -> begin
      remove_item "Vegetarian";
      remove_item "Vegan"
    end
  (*TODO this is Morrison and Appel*)
  | 7 -> begin
      remove_item "Morrison";
      remove_item "North Star";
      remove_item "Okensheilds"
    end
  | 8 -> begin
      remove_item "Morrison";
      remove_item "North Star";
      remove_item "Okensheilds"
    end
  (*TODO this is okenshields*)
  | 9 -> begin
      remove_item "Morrison";
      remove_item "North Star";
      remove_item "Okensheilds"
    end
  | _ -> print_endline "please input only one of the choices"

(* let morrison_foods filter_cat = let new_morrison_menu = filter
   "data/morrison.csv" filter_cat in *)

let print_by_dining_hall filtered_list =
  let new_morrison_menu = filter filtered_list (4, "Y") in
  print_endline "Morrison Dining options";
  print_endline (user_menu new_morrison_menu);
  let new_north_menu = filter filtered_list (5, "Y") in
  print_endline "North Star options";
  print_endline (user_menu new_north_menu);
  let new_oak_menu = filter filtered_list (6, "Y") in
  print_endline "Oakenshields options";
  print_endline (user_menu new_oak_menu)

let () =
  print_endline
    "Please choose what type of cuisine you would like to see,\n\
    \       what dietary  restriction you would like to filter by, or which \
     dining\n\
    \       hall you are  interested in: ";

  print_endline
    "Please choose what day you would like to see the menu for:\n       ";
  print_endline
    (* TODO 2) Tuesday \n\ 3) Wednesday \n\ 4) Thursday \n\ 5) Friday \n\ 6)
       Saturday \n\ 7) Sunday \n\*)
    " 1) Monday \n\n        ";
  let day_chosen = read_line () in
  let fileloaded = finding_day day_chosen in
  print_endline
    "Please choose what type of cuisine you would like to see, what dietary \
     restriction you would like to filter by, or if you want to leave \
     feedback: ";
  (*0 is for feedback, 1 - 6 are the cuisine/dietary stuff, 7-9 are dining
    halls, add okenshields later*)
  print_endline
    (* TODO 7) Morrison 8) Appel *)
    "0) View feedback/Leave a rating \n\
     1) Asian \n\
     2) American \n\
     3) Mexican \n\
     4) Italian \n\
     5) Vegetarian \n\
     6) Vegan \n";

  try
    let user_input = read_line () in
    let int_input = int_of_string user_input in
    (*todo use if int_input = 0 then handle_feedback () in future *)
    if int_input = 0 then handle_feedback ()
    else
      let filter_cat = finding_category (int_of_string user_input) in
      (*mutable current_menu that changes depending on what the user wants*)
      let current_menu = ref (Csv.load fileloaded) in 
      
      (*idea:
      for first thing chosen, all the remove_item function, filter with the thing
      and then 
      then prompt again to see if they would like another thing using the remaining things
      if nothing, then just display  *)

      (*these are the filtered menus for each dining hall*)
      let new_morrison_menu = filter morrison filter_cat in
      print_endline "Morrison Dining options";
      print_endline (user_menu new_morrison_menu);
      let new_appel_menu = filter appel filter_cat in
      print_endline "North Star options";
      print_endline (user_menu new_appel_menu)
  with
  | Unknown_Correspondence ->
      print_endline "please input only one of the choices"
  | Invalid_Csv "Invalid CSV format" -> print_endline "Invalid CSV format"
  | Failure _ -> print_endline "please input one of the choices"
  | Sys_error _ -> print_endline "please input a number for your selection"
