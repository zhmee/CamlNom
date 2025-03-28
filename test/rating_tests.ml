open Final_project.Dining
open Final_project.Rating
open QCheck




(**[make_rating (name, rating, hall, comments)] checks that the fields name,
   rating, hall, and comments are properly stored when converted to a [Rating.t]
   instance by [Rating.of_strings]*)
let make_rating (name, rating, hall, comments) =
  let r = of_strings name rating hall comments in
  get_name r = name
  && get_rating r = rating
  && get_hall r = hall
  && get_comments r = comments

(**[make_csv_entry name rating comments] checks that transforming the inputs
   [name] [rating] and [comments] to a readable csv format does not alter their
   inputs*)
let make_csv_entry (name, rating, hall, comments) =
  let r = to_csv_entry (of_strings name rating hall comments) in
  List.nth r 0 = name
  && List.nth r 1 = string_of_int rating
  && List.nth r 2 = hall
  && List.nth r 3 = comments

(**[get_name_test name rating comments] checks that the name is properly stored
   and returned, in string form, by [get_name]*)
let get_name_test (name, rating, hall, comments) =
  get_name (of_strings name rating hall comments) = name

(**[get_rating_test name rating comments] checks that the rating is properly
   stored and returned, in int form, by [get_rating]*)
let get_rating_test (name, rating, hall, comments) =
  get_rating (of_strings name rating hall comments) = rating

(**[get_comments_test (name, rating, hall, comments)] checks that the comments
   are properly stored and returned, in string form, by [get_comments]*)
let get_comments_test (name, rating, hall, comments) =
  get_comments (of_strings name rating hall comments) = comments

(**[get_hall_test (name, rating, hall, comments)] verifies that the dining hall
   is properly stored and returned in string form by [get_hall] *)
let get_hall_test (name, rating, hall, comments) =
  get_hall (of_strings name rating hall comments) = hall

(**[make_randomized_tuple func name] creates a randomized rating (consistng of a
   random string, an integer between 1 and 5, and another random string) and
   tests whether the test [func] is satisfied. *)
let make_randomized_tuple func name =
  QCheck.Test.make ~name ~count:1000
    (QCheck.make
       (QCheck.Gen.tup4 QCheck.Gen.string_printable (QCheck.Gen.int_range 1 5)
          QCheck.Gen.string_printable QCheck.Gen.string_printable)
       ~print:
         (QCheck.Print.tup4 QCheck.Print.string QCheck.Print.int
            QCheck.Print.string QCheck.Print.string))
    func

let example_ratings = Csv.load "../data/test_ratings.csv" ~strip:true
let name_asc = Csv.trim (Csv.load "../data/name_asc.csv" ~strip:true)
let name_desc = Csv.trim (Csv.load "../data/name_desc.csv" ~strip:true)
let hall_asc = Csv.trim (Csv.load "../data/hall_asc.csv" ~strip:true)
let hall_desc = Csv.trim (Csv.load "../data/hall_desc.csv" ~strip:true)
let rating_asc = Csv.trim (Csv.load "../data/rating_asc.csv" ~strip:true)
let rating_desc = Csv.trim (Csv.load "../data/rating_desc.csv" ~strip:true)

(**[check_equal_name] creates randomized tests verifying the sorting of the CSV
   files by name *)
let check_equal_name =
  QCheck.Test.make ~name:"Check sorting by name" ~count:1000 QCheck.bool
    (fun b ->
      Csv.print_readable (sort_name example_ratings b);
      print_newline ();
      Csv.compare
        (sort_name example_ratings b)
        (if b then (
           Csv.print_readable name_asc;
           name_asc)
         else (
           Csv.print_readable name_desc;
           name_desc))
      = 0)

(**[check_equal_hall] creates randomized tests verifying the sorting of the CSV
   files by hall *)
let check_equal_hall =
  QCheck.Test.make ~name:"Check sorting by hall" ~count:1000 QCheck.bool
    (fun b ->
      Csv.print_readable (sort_hall example_ratings b);
      print_newline ();
      Csv.compare
        (sort_hall example_ratings b)
        (if b then (
           Csv.print_readable hall_asc;
           hall_asc)
         else (
           Csv.print_readable hall_desc;
           hall_desc))
      = 0)

(**[check_equal_rating] creates randomized tests verifying the sorting of the
   CSV files by rating *)
let check_equal_rating =
  QCheck.Test.make ~name:"Check sorting by hall" ~count:1000 QCheck.bool
    (fun b ->
      Csv.print_readable (sort_rating example_ratings b);
      print_newline ();
      Csv.compare
        (sort_hall example_ratings b)
        (if b then (
           Csv.print_readable rating_asc;
           rating_asc)
         else (
           Csv.print_readable rating_desc;
           rating_desc))
      = 0)

let () =
  let _ =
    QCheck_runner.run_tests
      [
        make_randomized_tuple make_rating "Make a rating";
        make_randomized_tuple make_csv_entry "Make a CSV entry";
        make_randomized_tuple get_name_test "Get the name";
        make_randomized_tuple get_rating_test "Get the rating";
        make_randomized_tuple get_comments_test "Get the comments";
        make_randomized_tuple get_hall_test "Get dining hall";
        check_equal_name;
        check_equal_hall;
      ]
  in
  ()
