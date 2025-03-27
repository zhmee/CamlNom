type t = {
  name : string;
  rating : int;
  hall : string;
  comments : string;
}

exception Unknown_Input of string

let get_name r = r.name
let get_hall r = r.hall
let get_rating r = r.rating
let get_comments r = r.comments
let of_strings name rating hall comments = { name; rating; hall; comments }
let to_csv_entry r = [ r.name; string_of_int r.rating; r.hall; r.comments ]

(**[check_valid_num] takes in the input rating and makes sure that it is between
   1-5. Raises: Unknown_Input when the rating does not fall between the bounds.*)
let check_valid_num input =
  if input < 1 || input > 5 then
    raise (Unknown_Input "The rating is not within bounds")
  else input

(**[check_valid_bounds] takes in two inputs and makes sure both are in the
   needed range and that the first input is not less than the second. Raises:
   Unknown_Input if the inputs are not valid or are not within bounds.*)
let check_valid_bounds input1 input2 =
  if input1 < 1 || input2 > 5 then
    raise (Unknown_Input "The rating is not within bounds")
  else if input1 > input2 then
    raise (Unknown_Input "The rating is not within bounds")

(**[check_valid_halls] checks to see if what the user is looking for in the
   halls is one of the accepted ones. Raises: Unknown_Input if the dining hall
   is not recognized.*)
let check_valid_halls input =
  match input with
  | "Morrison" -> input
  | "North Star" -> input
  | "Okenshield" -> input
  | _ -> raise (Unknown_Input "The dining hall entered is not known")

let retrieve_ratings_name csv_file name =
  List.filter (fun lst -> List.nth lst 0 = name) csv_file

let retrieve_ratings_int csv_file rating =
  let valid_rating = check_valid_num rating in
  List.filter (fun lst -> List.nth lst 1 = string_of_int valid_rating) csv_file

let retrieve_ratings_int_range csv_file low high =
  check_valid_bounds low high;
  List.filter
    (fun lst ->
      let v = int_of_string (List.nth lst 1) in
      v >= low && v <= high)
    csv_file

let retrieve_ratings_hall csv_file hall =
  let valid_hall = check_valid_halls hall in
  List.filter (fun lst -> List.nth lst 2 = valid_hall) csv_file

let sort_name csv_file asc =
  List.sort
    (fun lst1 lst2 ->
      let c = compare (List.nth lst1 0) (List.nth lst2 0) in
      if c = 0 then compare lst1 lst2 else c * if asc then 1 else -1)
    csv_file

let sort_hall csv_file asc =
  List.sort
    (fun lst1 lst2 ->
      let c = compare (List.nth lst1 2) (List.nth lst2 2) in
      if c = 0 then compare lst1 lst2 else c * if asc then 1 else -1)
    csv_file

let sort_rating csv_file asc =
  List.sort
    (fun lst1 lst2 ->
      let c = compare (List.nth lst1 1) (List.nth lst2 1) in
      if c = 0 then compare lst1 lst2 else c * if asc then 1 else -1)
    csv_file

let rec print_ratings m =
  match m with
  | [] -> "\n"
  | h :: t -> String.concat " " h ^ "\n" ^ print_ratings t
