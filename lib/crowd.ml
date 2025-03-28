open Unix
open Options

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
  List.assoc day days_of_week

let get_rating rating = rating.value
