open Final_project
open Final_project.Dining

exception Unknown_Correspondence

let finding_category category = 
  match category with
  | 1 -> (1, "Asian")
  | 2 -> (1, "American")
  | 3 -> (1, "Mexican")
  | 4 -> (1, "Italian")
  | 5 -> (2, "Y")
  | 6 -> (3, "Y")
  | 7 -> (4, "Y")
  | 8-> (5, "Y")
  | 9 -> (6, "Y")
  | _  -> raise Unknown_Correspondence 



let () =
  print_endline "Please choose what type of cuisine you would like to see, what dietary restriction you would like to filter by, or which dining hall you are interested in: ";
 (*1 - 6 are the cuisine/dietary stuff, 7-9 are dining halls, add okenshields later*)
 print_endline "1) Asian 2) American 3) Mexican 4) Italian 5) Vegetarian 6) Vegan 7) Morrison 8) Appel"; 
 try 
 let user_input = read_line () in 
 let filter_cat = finding_category (int_of_string user_input) in 

 (*these are the filtered menus for each dining hall*)
 let new_morrison_menu = filter "data/morrison.csv" (filter_cat) in 
 print_endline ("Morrison Dining options");
  print_endline  ( user_menu new_morrison_menu);
  let new_appel_menu = filter "data/appel.csv" (filter_cat) in 
  print_endline ("North Star options");
  print_endline  (user_menu new_appel_menu);

with 
| Unknown_Correspondence -> print_endline "please input only one of the choices"
| Sys_error _ -> print_endline "please input a number for your selection"

 
 

