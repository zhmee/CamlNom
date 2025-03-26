open Csv

type t = string list list

exception Unknown_Selection of string

(**[check m f h a] is the helper for filter' to either append [h] to [acc] or
   return only [acc]. [m] is the value that needs to be checked, [f] is the filter
   condition, [h] is the row, [acc] is the accumulated rows already.*)
let check m f h acc = if m = snd f then h :: acc else acc

(**[filter' c f a] is the csv [c] filtered to include only food items that match
   the filter parameter [f]. [filt] has the (col, and the word).*)
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

let filter csv prefs = filter' (Csv.load csv) prefs []
let make_menu c = Csv.load c

(**[string_of_list m acc] is the string consisting of the elements in [m]
   appended to [acc]*)
let rec string_of_list m acc =
  match m with
  | [] -> acc
  | h :: t -> string_of_list t (acc ^ h)

(**[string_of_menu' m acc] is the helper function to make a string list list
   into a string using tail recursion*)
let rec string_of_menu' m acc =
  match m with
  | [] -> acc
  | h :: t -> string_of_menu' t (acc ^ string_of_list h "")

let string_of_menu m = string_of_menu' m ""
