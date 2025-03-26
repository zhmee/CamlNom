open Final_project.Dining
open Final_project.Rating
open QCheck

(**[make_rating name rating comments] checks that the fields name, rating, and
   comments are properly stored when converted to a [Rating.t] instance by
   [Rating.of_strings]*)
let make_rating (name, rating, comments) =
  let r = of_strings name rating comments in
  get_name r = name && get_rating r = rating && get_comments r = comments

(**[make_csv_entry name rating comments] checks that transforming the inputs
   [name] [rating] and [comments] to a readable csv format does not alter their
   inputs*)
let make_csv_entry (name, rating, comments) =
  let r = to_csv_entry (of_strings name rating comments) in
  List.nth r 0 = name
  && List.nth r 1 = string_of_int rating
  && List.nth r 2 = comments

(**[get_name_test name rating comments] checks that the name is properly stored
   and returned, in string form, by [get_name]*)
let get_name_test (name, rating, comments) =
  get_name (of_strings name rating comments) = name

(**[get_rating_test name rating comments] checks that the rating is properly
   stored and returned, in int form, by [get_rating]*)
let get_rating_test (name, rating, comments) =
  get_rating (of_strings name rating comments) = rating

(**[get_comments_test name rating comments] checks that the comments are
   properly stored and returned, in string form, by [get_comments]*)
let get_comments_test (name, rating, comments) =
  get_comments (of_strings name rating comments) = comments

(**[make_randomized_tuple func name] creates a randomized rating (consistng of a
   random string, an integer between 1 and 5, and another random string) and
   tests whether the test [func] is satisfied. *)
let make_randomized_tuple func name =
  QCheck.Test.make ~name ~count:1000
    (QCheck.make
       (QCheck.Gen.tup3 QCheck.Gen.string_printable (QCheck.Gen.int_range 1 5)
          QCheck.Gen.string_printable)
       ~print:
         (QCheck.Print.tup3 QCheck.Print.string QCheck.Print.int
            QCheck.Print.string))
    func

let () =
  let _ =
    QCheck_runner.run_tests
      [
        make_randomized_tuple make_rating "Make a rating";
        make_randomized_tuple make_csv_entry "Make a CSV entry";
        make_randomized_tuple get_name_test "Get the name";
        make_randomized_tuple get_rating_test "Get the rating";
        make_randomized_tuple get_comments_test "Get the comments";
      ]
  in
  ()
