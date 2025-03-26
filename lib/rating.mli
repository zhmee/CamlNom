type t
(**[t] represents the food that the user is leaving a rating on. *)

val get_name : t -> string
(**[get_food_name rating] returns the food that the user left a review of,
   represented as a [string]. *)

val get_rating : t -> int
(**[get_rating rating] returns the "stars" that the user gave the food,
   represented as an [int]. 1 star corresponds to a poor review, 5 stars
   correspond to a fantastic review. *)

val get_comments : t -> string
(**[get_comments rating] returns the comments that the user left in their
   rating, if any. The comments are returned as represented in a [string]*)

val of_strings : string -> int -> string -> string -> t
(**[of_strings name rating dining hall comments] creates a rating given the
   name, rating, dining hall and comments provided, of types [string], [int],
   [string], and [string], respectively. Requires: [rating] is an integer
   between 1 and 5. *)

val to_csv_entry : t -> string list
(**[to_csv_entry rating] returns [rating] represented as a CSV entry*)

val get_hall : t -> string
(**[get_hall rating] returns the associated dining hall the food was rated at*)
