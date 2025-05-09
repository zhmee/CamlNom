let halls = ref []
let dietary = ref [ "Vegetarian"; "Vegan" ]
let cuisines = ref (Csv.load "driver_data/cuisines.csv" |> List.map List.hd)
let get_all_filters () = ref (!halls @ !dietary @ !cuisines)

let days_of_week =
  [
    (0, "Sunday");
    (1, "Monday");
    (2, "Tuesday");
    (3, "Wednesday");
    (4, "Thursday");
    (5, "Friday");
    (6, "Saturday");
    (7, "Sunday");
  ]

let meal_types = ref []
let master_file_name = "./driver_data/recipe_book.csv"
let recipe_book = ref Dining.empty
let recipes = ref BatSet.empty

let meats =
  [
    "Beef";
    "Chicken";
    "Tuna";
    "Egg";
    "Pork";
    "Salmon";
    "Lamb";
    "Fish";
    "Bacon";
    "Ham";
    "Turkey";
    "Shrimp";
    "Duck";
  ]
