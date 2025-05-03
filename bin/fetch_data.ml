(**[pull_data.ml] contains the functions required to update the csv files
   automatically using Cornell Dining's website. Algorithm and functions adapted
   from this example/tutorial:
   https://www.scrapingbee.com/blog/ocaml-web-scraping/*)

module Fetch_data = struct
  open Cohttp_lwt_unix
  open Lwt_unix
  open Core
  open Yojson.Basic
  open BatSet
  open Yojson.Basic.Util
  open Core_unix
  open Final_project.Options

  let data_halls = ref empty

  let get_site =
    Lwt.bind
      (Client.get
         (Uri.of_string
            "https://now.dining.cornell.edu/api/1.0/dining/eateries.json"))
      (fun (_, x) -> x |> Cohttp_lwt.Body.to_string)

  let pull_data = Lwt_main.run get_site

  let add_items items recipe_list =
    if List.is_empty items then ()
    else
      List.iter items ~f:(fun item ->
          let recipe = item |> member "item" |> to_string in
          recipe_list := add recipe !recipe_list)

  let create_meal name date meal =
    let recipe_list = ref empty in
    let meal_name = meal |> member "descr" |> to_string in
    let menu = meal |> member "menu" |> to_list in
    List.iter menu ~f:(fun category ->
        add_items (category |> member "items" |> to_list) recipe_list);
    if BatSet.is_empty !recipe_list then ()
    else
      let csv_file =
        BatSet.to_list (BatSet.map (fun recipe -> [ recipe ]) !recipe_list)
      in
      let file_name = "dining_data/" ^ name ^ date ^ meal_name ^ ".csv" in
      Csv.save file_name csv_file;
      data_halls := add name !data_halls

  let day_csv name day_menu =
    let date = day_menu |> member "date" |> to_string in
    let meals = day_menu |> member "events" |> to_list in
    List.iter meals ~f:(create_meal name date)

  let scrape_data hall_json =
    let name = hall_json |> member "name" |> to_string in
    let operating_hours = hall_json |> member "operatingHours" |> to_list in
    List.iter operating_hours ~f:(day_csv name)

  let get_files =
    let dir = opendir "dining_data" in
    let rec add_file lst =
      match readdir_opt dir with
      | Some x -> add_file (x :: lst)
      | None -> lst
    in
    let x = add_file [] in
    List.iter x ~f:print_endline;
    x

  let fetch () =
    (try
       if BatUnix.is_directory "dining_data" then
         let files = get_files in
         List.iter files ~f:(fun file_name ->
             BatUnix.unlink ("dining_data/" ^ file_name))
       else BatUnix.mkdir "dining_data" 0o640
     with _ -> ());
    let json = from_string pull_data in
    let data = json |> member "data" in
    let halls = data |> member "eateries" |> to_list in
    List.iter halls ~f:scrape_data;
    Final_project.Options.halls := BatSet.to_list !data_halls
end
