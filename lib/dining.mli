type t
(**type of a menu*)

val filter : string -> int * string -> t
(**[filter c f] is the csv located at the path [c] filtered to include only
   lines that match the desired filter parameters [f]. Raises: Exception
   Unknown_Index when the input index is not recognized and Invalid_Csv when the
   csv format is incorrect.*)

val make_menu : string -> t
(**[make_menu c] is the menu made by using the csv located at the file path [c]*)

val string_of_menu : t -> string
(**[string_of_menu m] is the string representation of the menu [m]*)

val user_menu : t -> string
(**[user_menu m] is the menu printed out in a user friendly output*)

exception Unknown_Index of string
(**Raised when the index to be filtered for is not recognized*)

exception Invalid_Csv of string
(**Raised when the csv input is not rectangular*)
