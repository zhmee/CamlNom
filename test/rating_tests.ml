open Final_project.Dining
open Final_project.Rating

(**[make_rating name rating comments] checks that the fields name, rating, and
   comments are properly stored when converted to a [Rating.t] instance by
   [Rating.of_strings]*)
let make_rating name rating comments =
  let r = of_strings name rating comments in
  get_name r = name && get_rating r = rating && get_comments r = comments

(**[make_csv_entry name rating comments] checks that transforming the inputs
   [name] [rating] and [comments] to a readable csv format does not alter their
   inputs*)
let make_csv_entry name rating comments =
  let r = to_csv_entry (of_strings name rating comments) in
  List.nth r 0 = name
  && List.nth r 1 = string_of_int rating
  && List.nth r 2 = comments

(**[get_name name rating comments] checks that the name is properly stored and
   returned, in string form, by [get_name]*)
let get_name_test name rating comments =
  get_name (of_strings name rating comments) = name

(**[get_rating_test name rating comments] checks that the rating is properly
   stored and returned, in int form, by [get_rating]*)
let get_rating_test name rating comments =
  get_rating (of_strings name rating comments) = rating

(**[get_comments name rating comments] checks that the comments are properly
   stored and returned, in string form, by [get_comments]*)
let get_comments_test name rating comments =
  get_comments (of_strings name rating comments) = comments
