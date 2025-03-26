type t = {
  name : string;
  rating : int;
  comments : string;
}

let get_name r = r.name
let get_rating r = r.rating
let get_comments r = r.comments
let of_strings name rating comments = { name; rating; comments }
let to_csv_entry r = [ r.name; string_of_int r.rating; r.comments ]
