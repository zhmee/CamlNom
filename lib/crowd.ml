open Unix

type t = {
  hall : string;
  value : int;
  day_time : tm;
}

let of_inputs hall value time = { hall; value; day_time = localtime time }

let get_time rating =
  let time = rating.day_time in
  (time.tm_hour, time.tm_min, time.tm_sec)

let get_hall rating = rating.hall

let get_day rating =
  let day = rating.day_time.tm_wday in
  match day with
  | 0 -> "Sunday"
  | 1 -> "Monday"
  | 2 -> "Tuesday"
  | 3 -> "Wednesday"
  | 4 -> "Thursday"
  | 5 -> "Friday"
  | _ -> "Saturday"

let get_rating rating = rating.value
