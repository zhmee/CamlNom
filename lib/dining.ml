type t = string list list

(**[check m f h a] is the helper for filter' to either append [h] to [acc] or
   return only [acc]*)
let check m f h acc = if m = snd f then h :: acc else acc

(**[filter' c f a] is the csv [c] filtered to include only food items that match
   the filter parameter [f]*)
let rec filter' (csv : t) (filt : int * string) (acc : t) =
  match csv with
  | [] -> acc (*reached end of csv file*)
  | h :: t ->
      filter' t filt
        (match h with
        | [ name; cuisine; vege; vegan; m; a; o ] -> (
            match fst filt with
            | 0 -> check name filt h acc
            | 1 -> if cuisine = snd filt then h :: acc else acc
            | 2 -> check vege filt h acc
            | 3 -> check vegan filt h acc
            | 4 -> check m filt h acc
            | 5 -> check a filt h acc
            | 6 -> check o filt h acc
            | _ -> failwith "index of filter selection not found")
        | _ -> failwith "invalid csv format")

let filter csv prefs = filter' csv prefs []
