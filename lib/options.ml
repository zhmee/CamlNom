let halls = ref [ "Morrison"; "Appel"; "Oakenshields" ]
let dietary = ref [ "Vegetarian"; "Vegan" ]
let cuisines = ref [ "Asian"; "American"; "Mexican"; "Italian" ]
let get_all_filters = ref (!halls @ !dietary @ !cuisines)
