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
