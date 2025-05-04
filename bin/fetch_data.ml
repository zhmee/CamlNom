(**[pull_data.ml] contains the functions required to update the csv files
   automatically using Cornell Dining's website. Algorithm and functions adapted
   from this example/tutorial:
   https://www.scrapingbee.com/blog/ocaml-web-scraping/*)

(**information regarding campus eatery menus scraped from the Cornell Open Data
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

module Fetch_data = struct
  (**[data_halls] keeps track of the different halls around campus specified by
     the webscraped JSON. This is a reference type used to update
     [Options.halls]*)
  let data_halls = ref BatSet.empty

  (**[meals] keeps track of the different types of meals (e.g. Lunch or Dinner)
     offered at each eatery around campus, in the form of an association list.*)
  let meals = ref []

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

  (**[new_recipe recipe] generates a new [string list] representing the
     specifics of the recipe represented by [recipe]. Requires: [recipe] is a
     new addition; it belongs to [Options.recipes] but not [Options.recipe_book]*)
  let new_recipe recipe =
    let specs = ref [] in
    print_endline ("Please insert information about " ^ recipe ^ ". \n");
    List.iter
      (fun restriction ->
        print_endline ("Is this dish " ^ restriction ^ "? (Y/N)");
        let input = read_line () in
        if input = "Y" || input = "N" then specs := input :: !specs
        else (
          print_endline "Illegal input entered. A \"N\" is assumed.";
          specs := input :: !specs))
      !dietary;
    print_endline "What cuisine is this dish?";
    let input = read_line () in
    specs := input :: !specs;
    if not (List.mem input !cuisines) then cuisines := input :: !cuisines
    else ();
    List.iter
      (fun hall ->
        print_endline ("Is this dish found at " ^ hall ^ "? (Y/N)");
        let input = read_line () in
        if input = "Y" || input = "N" then specs := input :: !specs
        else (
          print_endline "Illegal input entered. A \"N\" is assumed.";
          specs := input :: !specs))
      !halls;
    !specs

  (**[pull_specific_data recipe] takes the recipe with name [recipe] and returns
     the [list] containing the specific information, such as dietary
     restrictions and cuisine. If [recipe] is not an existing recipe in
     [Options.recipes] then a new recipe card is added. *)
  let pull_specific_data recipe =
    if mem recipe !recipes then ()
    else (
      recipes := BatSet.add recipe !recipes;
      let c = new_recipe recipe in
      recipe_book := add_recipe !recipe_book c;
      List.iter print_string c)

  (**[create_meal name date meal] creates the csv file corresponding to a
     singular meal (e.g. Lunch or Dinner) at the dining hall with name [name] on
     date [date]. Requires: [meal] consists of the JSON object specific to hall
     [name] on date [date]. Moreover, [name] must be a valid dining hall
     belonging to [!Options.halls]. *)
  let create_meal name date meal =
    let recipe_list = ref BatSet.empty in
    let meal_name = meal |> member "descr" |> to_string in
    let menu = meal |> member "menu" |> to_list in
    List.iter
      (fun category ->
        add_items (category |> member "items" |> to_list) recipe_list)
      menu;
    if BatSet.is_empty !recipe_list then ()
    else BatSet.iter pull_specific_data !recipe_list;
    let csv_file =
      Final_project.Dining.find_recipes !recipe_list !recipe_book
    in
    BatSet.iter print_endline !recipe_list;
    print_endline "HERE";
    print_endline (string_of_menu !recipe_book);

    let file_name = "dining_data/" ^ name ^ date ^ meal_name ^ ".csv" in
    save_menu csv_file file_name;

    data_halls := add name !data_halls;
    let hall_meals = BatList.assoc name !meals in
    hall_meals := add meal_name !hall_meals

  (**[day_csv name day_menu] creates the CSV files corresponding to a singular
     day at the hall specified by [name]. Requires: [day_menu] is a JSON with
     the relevant information specific to the hall [name]*)
  let day_csv name day_menu =
    let date = day_menu |> member "date" |> to_string in
    let meals = day_menu |> member "events" |> to_list in
    List.iter (create_meal name date) meals

  (**[scrape_data hall_json] reads the information regarding the different menus
     in eateries across campus from a webscraped JSON object [hall_json].
     Requires: [hall_json] contains the relevant information mentioned. *)
  let scrape_data hall_json =
    let name = hall_json |> member "name" |> to_string in
    let operating_hours = hall_json |> member "operatingHours" |> to_list in
    meals := (name, ref BatSet.empty) :: !meals;
    List.iter (day_csv name) operating_hours

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

  (**[fill_master_list ()] updates the master list contained in [Options.ml]
     with the updated information regarding which recipes Cornell Dining can
     make. *)
  let fill_master_list () =
    let master_file_name = master_file_name in
    recipe_book := make_menu master_file_name;
    print_endline (string_of_menu !recipe_book);
    List.iter
      (fun row -> recipes := BatSet.add (List.hd row) !recipes)
      (to_csv !recipe_book)

  (**[fetch ()] updates the directory [./dining_data], which contains updated
     information on the menus in each dining hall. *)
  let fetch () =
    fill_master_list ();
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
    let halls = data |> member "eateries" |> to_list in
    List.iter scrape_data halls;
    Final_project.Options.halls := BatSet.to_list !data_halls;
    meal_types :=
      List.map
        (fun (hall_name, hall_meals) ->
          (hall_name, ref (BatSet.to_list !hall_meals)))
        !meals
end
