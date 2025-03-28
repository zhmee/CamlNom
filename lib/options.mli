val halls : string list ref
(**[halls] keeps track of the different dining halls that are supported by this
   project*)

val cuisines : string list ref
(**[cuisines] keeps track of the different cuisines that are supported by this
   project. *)

val dietary : string list ref
(**[dietary] keeps track of the different dietary restrictions that are
   supported by this project. *)

val get_all_filters : string list ref
(**[get_all_filters] returns a comprehensive list of all the filters that are
   supported by this project. *)
