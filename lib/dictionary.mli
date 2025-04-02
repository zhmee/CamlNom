type t
(**The type representing the dictionary of foods. *)

val transform_to_list : t -> string list
(**[transform_to_list] Transforms the dictionary into a list of foods. *)

val check_spelling : string -> t -> bool
(**[check_spelling] checks to see if a word is in the dictionary. *)

val remove_dup : t -> string list
(**[remove_dup] removes all the duplicate words in the dictionary *)

val corrected_word : string -> t -> string
(**[corrected_word] takes in a word and returns the corrected spelling version
   of it. *)
