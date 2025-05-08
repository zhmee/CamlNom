(**[pull_data.ml] contains the functions required to update the csv files
   automatically using Cornell Dining's website. Algorithm and functions adapted
   from this example/tutorial:
   https://www.scrapingbee.com/blog/ocaml-web-scraping/*)

(*information regarding campus eatery menus scraped from the Cornell Open Data
  Initiative, found at
  https://codi.engineering.cornell.edu/data/diningdata.html*)

open Cohttp_lwt_unix
open Lwt_unix
open Yojson.Basic
open BatSet
open Yojson.Basic.Util
open Core_unix
open Final_project.Options
open Final_project.Dining
open String

module Fetch_data = struct
  (**[data_halls] keeps track of the different halls around campus specified by
     the webscraped JSON. This is a reference type used to update
     [Options.halls]*)
  let data_halls = ref BatSet.empty

  (**[meals] keeps track of the different types of meals (e.g. Lunch or Dinner)
     offered at each eatery around campus, in the form of an association list.*)
  let meals = ref []

  (**[queries] keeps track of the recipes at each dining hall at each meal, to
     query later. *)
  let queries = ref []

  (**[recipe_book_adj] keeps track of the current menu using an adjustable CSV
     that can add new columns or change column entries*)
  let recipe_book_adj = ref (Final_project.Dining.empty |> to_adjustable)

  (**[get_site] webscrapes the data containing the menu information from the
     Cornell Open Data Project.*)
  let get_site =
    Lwt.bind
      (Client.get
         (Uri.of_string
            "https://now.dining.cornell.edu/api/1.0/dining/eateries.json"))
      (fun (_, x) -> x |> Cohttp_lwt.Body.to_string)

  (**[pull_data] initiates the data transfer from the Cornell Dining website.*)
  let pull_data = Lwt_main.run get_site

  (**[master_list] is an empty list that will be updated as the file is run. *)
  let master_list = ref Final_project.Dining.empty

  (**[add_items item recipe_list] reads all the recipes contained in the JSON
     represented by [items] and returns their concatenation. Requires: [items]
     contains a list of only [strings]. *)
  let add_items items recipe_list =
    if List.is_empty items then ()
    else
      List.iter
        (fun item ->
          let recipe = item |> member "item" |> to_string in
          recipe_list := add recipe !recipe_list)
        items

  (**[contains str substring] checks whether [substring] is a substring of
     [str].*)
  let contains (str : string) (substring : string) =
    let r = Str.regexp substring in
    try
      let _ = Str.search_forward r str 0 in
      true
    with _ -> false

  (**[new_recipe recipe headers] generates a new [string list] representing the
     specifics of the recipe represented by [recipe]. Requires: [recipe] is a
     new addition; it belongs to [Options.recipes] but not [Options.recipe_book]*)
  let new_recipe recipe headers hall =
    let specs = ref [| recipe |] in
    print_endline ("Please insert information about " ^ recipe ^ ". \n");
    List.iter
      (fun filter ->
        if filter = "Cuisine" then (
          print_endline "What cuisine is this dish?";
          let input = read_line () in
          specs := Array.append !specs [| input |];
          if not (List.mem input !cuisines) then cuisines := input :: !cuisines
          else ())
        else if contains recipe filter then
          specs := Array.append !specs [| "Y" |]
        else if
          (filter = "Vegan" || filter = "Vegetarian")
          && List.exists (contains recipe) Final_project.Options.meats
        then specs := Array.append !specs [| "Y" |]
        else if filter = hall then specs := Array.append !specs [| "Y" |]
        else (
          print_endline ("Does the label \"" ^ filter ^ "\" apply? (Y/N)");
          let input = read_line () in
          if input = "Y" || input = "N" then
            specs := Array.append !specs [| input |]
          else (
            print_endline "Illegal input entered. A \"N\" is assumed.";
            specs := Array.append !specs [| input |])))
      (List.tl !headers);
    !specs

  (**[pull_specific_data headers orig_headers hall recipe ] takes the recipe
     with name [recipe] and returns the [list] containing the specific
     information, such as dietary restrictions and cuisine. If [recipe] is not
     an existing recipe in [Options.recipes] then a new recipe card is added. *)
  let pull_specific_data headers orig_headers hall recipe =
    if BatSet.mem recipe !recipes then ()
    else (
      recipes := BatSet.add recipe !recipes;
      let c = new_recipe recipe headers hall in
      recipe_book_adj := add_adjustable !recipe_book_adj c)

  (**[create_meal name date meal headers] creates the csv file corresponding to
     a singular meal (e.g. Lunch or Dinner) at the dining hall with name [name]
     on date [date]. Requires: [meal] consists of the JSON object specific to
     hall [name] on date [date]. Moreover, [name] must be a valid dining hall
     belonging to [!Options.halls]. *)
  let create_meal name date headers meal =
    let recipe_list = ref BatSet.String.empty in
    let meal_name = meal |> member "descr" |> to_string in
    if meal_name <> "Dinner" || name <> "104West!" || date <> "2025-05-08" then
      ()
    else
      let menu = meal |> member "menu" |> Yojson.Basic.Util.to_list in
      List.iter
        (fun category ->
          add_items
            (category |> member "items" |> Yojson.Basic.Util.to_list)
            recipe_list)
        menu;
      if BatSet.String.is_empty !recipe_list then ()
      else
        let file_name = "dining_data/" ^ name ^ date ^ meal_name ^ ".csv" in
        queries := (file_name, (name, recipe_list)) :: !queries;

        data_halls := BatSet.add name !data_halls;
        if not (List.mem name !headers) then headers := !headers @ [ name ]
        else ();
        let hall_meals = BatList.assoc name !meals in
        hall_meals := add meal_name !hall_meals

  (**[save_all meal_lst headers] takes the association [meal_list] (which
     contains a file name as the key and a set with the relevant recipe names as
     the value) and creates files for every element of the list*)
  let save_all meal_lst headers =
    recipe_book_adj :=
      !recipe_book_adj |> to_adjustable_csv |> List.tl |> from_adjustable_csv;
    List.iter
      (fun (file_name, (hall, recipes)) ->
        BatSet.String.iter
          (pull_specific_data headers
             (!recipe_book_adj |> to_adjustable_csv |> List.hd |> Array.to_list)
             hall)
          !recipes;
        let csv_file =
          Final_project.Dining.find_recipes !recipes
            (!recipe_book_adj |> from_adjustable)
        in
        List.iter print_endline !headers;
        save_menu csv_file headers file_name)
      meal_lst

  (**[day_csv name day_menu headers] creates the CSV files corresponding to a
     singular day at the hall specified by [name]. Requires: [day_menu] is a
     JSON with the relevant information specific to the hall [name]*)
  let day_csv name headers day_menu =
    Yojson.Basic.Util.(
      let date = day_menu |> member "date" |> to_string in
      let meals = day_menu |> member "events" |> Yojson.Basic.Util.to_list in
      List.iter (create_meal name date headers) meals)

  (**[scrape_data hall_json headers] reads the information regarding the
     different menus in eateries across campus from a webscraped JSON object
     [hall_json]. Requires: [hall_json] contains the relevant information
     mentioned. *)
  let scrape_data headers hall_json =
    Yojson.Basic.Util.(
      let name = hall_json |> member "name" |> to_string in
      let operating_hours =
        hall_json |> member "operatingHours" |> Yojson.Basic.Util.to_list
      in
      meals := (name, ref BatSet.String.empty) :: !meals;
      List.iter (day_csv name headers) operating_hours)

  (**[get_files] returns the subfiles contained in [./dining_data], each of
     which must be a file; there cannot be subdirectories within the folder.*)
  let get_files =
    let dir = opendir "dining_data" in
    let rec add_file lst =
      match readdir_opt dir with
      | Some x -> add_file (x :: lst)
      | None -> lst
    in
    let x = add_file [] in
    x

  (**[fill_master_list headers ()] updates the master list contained in
     [Options.ml] with the updated information regarding which recipes Cornell
     Dining can make. *)
  let fill_master_list headers () =
    recipe_book_adj := make_adjustable_menu master_file_name;
    List.iter
      (fun row -> recipes := BatSet.add row.(0) !recipes)
      (!recipe_book_adj |> to_adjustable_csv |> List.tl);
    headers := !recipe_book_adj |> to_adjustable_csv |> List.hd |> Array.to_list

  let write_master_list headers () =
    Csv.save master_file_name
      (!headers
      :: (Final_project.Dining.from_adjustable !recipe_book_adj
         |> Final_project.Dining.to_csv))

  (**[fetch ()] updates the directory [./dining_data], which contains updated
     information on the menus in each dining hall. *)
  let fetch () =
    let headers = ref [] in
    fill_master_list headers ();
    (try
       if BatUnix.is_directory "dining_data" then
         let files = get_files in
         List.iter
           (fun file_name -> BatUnix.unlink ("dining_data/" ^ file_name))
           files
       else BatUnix.mkdir "dining_data" 0o640
     with _ -> ());
    let json = from_string pull_data in
    let data = json |> member "data" in
    let halls = data |> member "eateries" |> Yojson.Basic.Util.to_list in
    List.iter (scrape_data headers) halls;
    Final_project.Options.halls := BatSet.to_list !data_halls;
    meal_types :=
      List.map
        (fun (hall_name, hall_meals) ->
          (hall_name, ref (BatSet.String.to_list !hall_meals)))
        !meals;
    save_all !queries headers;
    write_master_list headers ()
end
