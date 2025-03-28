open Csv

type t = string list list

exception Unknown_Index of string
exception Invalid_Csv of string

(**[check m f h a] is the helper for filter' to either append [h] to [acc] or
   return only [acc]*)
let check m f h acc = if m = snd f then h :: acc else acc

(**[filter' c f a] returns the csv [c] filtered to include only food items that
   match the filter parameter [f]. *)
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
            | _ -> raise (Unknown_Index "Incorrect choice"))
        | _ -> raise (Invalid_Csv "Invalid CSV format"))

(**[filter csv prefs] returns the filtered entries of [csv] that match [prefs].
   Requires: [prefs] is a tuple of type [int * string], with the first argument
   being an integer from 0 through 6, representing the category of the csv file
   (check filter' for specifications) and the second argument being a setting.
   For instance, [prefs = (0, "Apple")] returns all the dining options where
   [name = "Apple"]. Requires: [csv] is a valid dining file with entries of the
   form [string;string;(Y/N);(Y/N);(Y/N);(Y/N);(Y/N)]*)
let filter csv prefs = filter' csv prefs []

(**[make_menu c] loads the list of dining options available for a day from file
   path [c]. Requires: [c] is a file path to a valid dining csv file. See
   [filter] documentation for specification.*)
let make_menu c = Csv.load c

(**[string_of_list m acc] is the string consisting of the elements in [m]
   appended to [acc]. Requires [m] is a string list*)
let rec string_of_list m acc = List.fold_left ( ^ ) acc m

(**[string_of_menu' m acc] is the helper function to make a string list list
   into a string using tail recursion*)
let rec string_of_menu' m acc =
  List.fold_left (fun acc lst -> acc ^ string_of_list lst "") acc m

(**[string_of_menu m] returns the menu [m] represented as a string. Requires:
   [m] is a CSV file fitting the specifications noted in [filter] documentation.
*)
let string_of_menu m = string_of_menu' m ""

(**[user_menu m acc] is the helper function to make a string list list into a
   string *)
let rec user_menu m =
  match m with
  | [] -> "\n"
  | (h :: _) :: t -> h ^ "\n" ^ user_menu t
  | [] :: t -> user_menu t
