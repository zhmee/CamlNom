type t = string list list

exception Failure_File_not_found of string

let transform_to_list (dictionary : t) = List.flatten dictionary

let check_spelling word dictionary =
  let new_dictionary = transform_to_list dictionary in
  List.mem word new_dictionary

let remove_dup dictionary =
  let new_dictionary = transform_to_list dictionary in
  List.sort_uniq compare new_dictionary


let corrected_word word dictionary =
  if check_spelling word dictionary then word
  else
    let used_dictionary = remove_dup dictionary in
    let list_of_corrections =
      List.filter
        (fun check -> BatString.edit_distance check word = 1)
        used_dictionary
    in
    match list_of_corrections with
    | [] -> word
    | [ item ] -> item
    | h :: t -> h
