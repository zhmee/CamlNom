type t

val filter : string list list -> int * string -> t
(**[filter c f] is the csv [c] filtered to include only lines that match the
   desired filter parameters [f].*)
