type t
(**[Crowd.t] represents one user-inputted crowdedness rating.*)

val of_inputs : string -> int -> float -> t
(**[of_inputs hall value time] returns a crowdedness rating based off [hall],
   which is a dining hall, [value], an [int] representing how crowded, on a
   scale from 1 (least crowded) to 10 (no seating available) the dining hall is,
   and [time], a float representing the time elapsed since Jan 1 1970 to when
   the rating was entered*)

val get_hall : t -> string
(**[get_hall rating] returns the dining hall a particular rating was inputted
   from, representable as a string. *)

val get_time : t -> int * int * int
(**[get_time rating] returns the time of day that a particular rating [rating]
   was entered, representable as a [int * int * int] tuple (hour, minute,
   second)*)

val get_day : t -> string
(**[get_day rating] returns the day of the week a particular rating [rating] was
   entered, represented as a [string]*)

val get_rating : t -> int
(**[get_rating rating] returns the scale of crowdedness (an [int] between 1 and
   10, inclusive) that is documented in a particular rating*)
