let halls = ref []
let dietary = ref [ "Vegetarian"; "Vegan" ]
let cuisines = ref [ "Asian"; "American"; "Mexican"; "Italian" ]
let get_all_filters = ref (!halls @ !dietary @ !cuisines)

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
