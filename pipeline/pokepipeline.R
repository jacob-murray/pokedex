# pokemon 151
pokemon_choices <- c("Bulbasaur","Ivysaur","Venusaur",
                     "Charmander","Charmeleon","Charizard","Squirtle",
                     "Wartortle","Blastoise","Caterpie","Metapod",
                     "Butterfree","Weedle","Kakuna","Beedrill",
                     "Pidgey","Pidgeotto","Pidgeot","Rattata",
                     "Raticate","Spearow","Fearow","Ekans","Arbok",
                     "Pikachu","Raichu","Sandshrew","Sandslash",
                     "Nidoking","Clefairy","Clefable","Vulpix","Ninetales",
                     "Jigglypuff","Wigglytuff","Zubat","Golbat","Oddish",
                     "Gloom","Vileplume","Paras","Parasect","Venonat",
                     "Venomoth","Diglett","Dugtrio","Meowth","Persian","Psyduck",
                     "Golduck","Mankey","Primeape","Growlithe","Arcanine",
                     "Poliwag","Poliwhirl","Poliwrath","Abra","Kadabra",
                     "Alakazam","Machop","Machoke","Machamp","Bellsprout",
                     "Weepinbell","Victreebel","Tentacool","Tentacruel",
                     "Geodude","Graveler","Golem","Ponyta","Rapidash",
                     "Slowpoke","Slowbro","Magnemite","Magneton","Farfetchd",
                     "Doduo","Dodrio","Seel","Dewgong","Grimer","Muk","Shellder",
                     "Cloyster","Gastly","Haunter","Gengar","Onix","Drowzee",
                     "Hypno","Krabby","Kingler","Voltorb","Electrode",
                     "Exeggcute","Exeggutor","Cubone","Marowak","Hitmonlee",
                     "Hitmonchan","Lickitung","Koffing","Weezing","Rhyhorn",
                     "Rhydon","Chansey","Tangela","Kangaskhan","Horsea","Seadra",
                     "Goldeen","Seaking","Staryu","Starmie","Mr-Mime","Scyther",
                     "Jynx","Electabuzz","Magmar","Pinsir","Tauros","Magikarp",
                     "Gyarados","Lapras","Ditto","Eevee","Vaporeon","Jolteon",
                     "Flareon","Porygon","Omanyte","Omastar","Kabuto","Kabutops",
                     "Aerodactyl","Snorlax","Articuno","Zapdos","Moltres","Dratini",
                     "Dragonair","Dragonite","Mewtwo","Mew")

# fetching data for gen 1
for(name in pokemon_choices){
	print(name)
  pokemon <- stringr::str_to_lower(name)
  url <- glue::glue('https://pokeapi.co/api/v2/pokemon/{stringr::str_to_lower(pokemon)}')
  result <- httr::GET(url)
  data <- jsonlite::fromJSON(httr::content(result,"text"),flatten=TRUE)
  jsonlite::write_json(data, glue::glue("data/{pokemon}.json"))
}
