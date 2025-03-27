type t = {
  name : string;
  rating : int;
  hall : string;
  comments : string;
}

let get_name r = r.name
let get_hall r = r.hall
let get_rating r = r.rating
let get_comments r = r.comments
let of_strings name rating hall comments = { name; rating; hall; comments }
let to_csv_entry r = [ r.name; string_of_int r.rating; r.hall; r.comments ]

let retrieve_ratings_name csv_file name =
  List.filter (fun lst -> List.nth lst 0 = name) csv_file

let retrieve_ratings_int csv_file rating =
  List.filter (fun lst -> List.nth lst 1 = string_of_int rating) csv_file

let retrieve_ratings_int_range csv_file low high =
  List.filter
    (fun lst ->
      let v = int_of_string (List.nth lst 1) in
      v >= low && v <= high)
    csv_file

let retrieve_ratings_hall csv_file hall =
  List.filter (fun lst -> List.nth lst 2 = hall) csv_file

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
    | (h :: _) :: t -> h ^ "\n" ^ print_ratings t
    | [] :: t -> print_ratings t