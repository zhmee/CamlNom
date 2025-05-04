type t
(**type of a menu*)

val empty : t
(**[empty] represents the empty menu with no additional foods.*)

val filter : t -> int * string -> t
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

val length_of_menu : t -> int
(**[lenght_of_menu] takes in a menu and retrieves how many entries are in it*)

val list_of_menu : t -> string list
(**[list_of_menu] takes in a menu and returns the first entries in a list *)

val save_menu : t -> string -> unit
(**[save_menu] takes in a menu, a file name, and saves the menu to the specified
   file.*)

val add_recipe : t -> string list -> t
(**[add_recipe menu recipe] extends a recipe list [menu] by adding a new recipe
   [recipe], which contains all information in the form of a [string list]*)

val find_recipes : string BatSet.t -> t -> t
(**[find_recipes recipe_list recipe_book] returns the entries of the
   [recipe_book] including the recipe specifications (e.g. dietary restrictions
   or cuisine) of the recipes specified in [recipe_list]. Requires: all elements
   of [recipe_list] have a corresponding [recipe_book] entry, but not
   necessarily vice versa. *)

val to_csv : t -> Csv.t
(**[to_csv] returns a [Csv.t] representation of the dining menu. *)

exception Invalid_Csv of string
(**Raised when the csv input is not rectangular*)
