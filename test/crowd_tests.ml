open Final_project.Crowd
open QCheck
open Unix

let make_of_inputs_check_hall (hall, value) =
  let x = of_inputs hall value (time ()) in
  get_hall x = hall

let make_of_inputs_check_value (hall, value) =
  let x = of_inputs hall value (time ()) in
  get_rating x = value

let make_get_hour (hall, value) =
  let x = time () in
  let hour, _, _ = get_time (of_inputs hall value x) in
  (localtime x).tm_hour = hour

let make_get_minute (hall, value) =
  let x = time () in
  let _, min, _ = get_time (of_inputs hall value x) in
  (localtime x).tm_min = min

let make_get_sec (hall, value) =
  let x = time () in
  let _, _, sec = get_time (of_inputs hall value x) in
  (localtime x).tm_sec = sec
