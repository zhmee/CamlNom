open Final_project.Crowd
open QCheck
open Unix
open Final_project.Options

(**[days_of_week] uses the options provided in the enum file for convenience.*)
let days_of_week = Final_project.Options.days_of_week

(**[make_of_inputs_check_hall (hall, value)] verifies that creating a
   crowdedness rating for dining hall [hall] preserves the name of the hall that
   was listed. *)
let make_of_inputs_check_hall (hall, value) =
  let x = of_inputs hall value (time ()) in
  get_hall x = hall

(**[make_of_inputs_check_value (hall, value)] verifies that creating a rating
   preserves the user-inputted crowdedness value*)
let make_of_inputs_check_value (hall, value) =
  let x = of_inputs hall value (time ()) in
  get_rating x = value

(**[make_get_hour (hall, value, time)] verifies that the correct hour is
   returned when the rating is created*)
let make_get_hour (hall, value, time) =
  let hour, _, _ = get_time (of_inputs hall value time) in
  (localtime time).tm_hour = hour

(**[make_get_minute (hall, value, time)] verifies that the correct minute is
   returned when the rating is created*)
let make_get_minute (hall, value, time) =
  let _, min, _ = get_time (of_inputs hall value time) in
  (localtime time).tm_min = min

(**[make_get_sec (hall, value, time)] ensures that the correct second is
   returned when the rating is created*)
let make_get_sec (hall, value, time) =
  let _, _, sec = get_time (of_inputs hall value time) in
  (localtime time).tm_sec = sec

(**[make_get_day (hall, value, time)] ensures that the correct day is returned,
   as per the enum in the options file, when the rating is created*)
let make_get_day (hall, value, time) =
  let x = (localtime time).tm_wday in
  List.assoc x days_of_week = get_day (of_inputs hall value time)

(**[test_tup2 func name] is a helper function that generates a QCheck randomized
   test with a random dining hall (any random string) and a random rating, and
   runs the test [func] with name [name]*)
let test_tup2 func name =
  QCheck.Test.make ~name ~count:1000
    (QCheck.tup2 QCheck.printable_string (QCheck.int_range 1 10))
    func

(**[test_tup3 func name] is a helper function that generates a QCheck randomized
   test with a random dining hall (any random string), a random rating, and a
   random time, and runs the test [func] with name [name]*)
let test_tup3 func name =
  QCheck.Test.make ~name ~count:1000
    (QCheck.tup3 QCheck.printable_string (QCheck.int_range 1 10) QCheck.float)
    func

let () =
  QCheck_runner.run_tests_main
    [
      test_tup2 make_of_inputs_check_hall "Check creation hall";
      test_tup2 make_of_inputs_check_value "Check creation value";
      test_tup3 make_get_day "Get day";
      test_tup3 make_get_hour "Get hour";
      test_tup3 make_get_minute "Get minute";
      test_tup3 make_get_sec "Get second";
    ]
