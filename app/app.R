# libraries
library(dplyr)
library(jsonlite)
library(shiny)
library(bslib)

# color dictionary
color_dictionary <- c(
	'normal'='#A8A77A',
	'fire'='#EE8130',
	'water'='#6390F0',
	'electric'='#F7D02C',
	'grass'='#7AC74C',
	'ice'='#96D9D6',
	'fighting'='#C22E28',
	'poison'='#A33EA1',
	'ground'='#E2BF65',
	'flying'='#A98FF3',
	'psychic'='#F95587',
	'bug'='#A6B91A',
	'rock'='#B6A136',
	'ghost'='#735797',
	'dragon'='#6F35FC',
	'dark'='#705746',
	'steel'='#B7B7CE',
	'fairy'='#D685AD'
)

pokemon_choices <- c("Bulbasaur","Ivysaur","Venusaur", "Charmander",
                     "Charmeleon","Charizard","Squirtle",
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

# ui
ui <- page_sidebar(
  title = "Pokedex",
  sidebar = sidebar(
    selectInput(
      "pokemon_name", "Select a Pokmeon",
      choices = pokemon_choices
    )
  ),
  card(
    card_header(
      textOutput("pokemon_id"),
    ),
    card_body(
      textOutput("base_stats")
    )
  )
)

server <- function(input, output, session){
  
  dex <- reactive({
    req(input$pokemon_name)
    dex <- jsonlite::read_json(glue::glue("data/{stringr::str_to_lower(input$pokemon_name)}.json"))
    output <- as.character(dex$id)
  })

  output$pokemon_id <- renderText({
    input$pokemon_name
  })

  output$base_stats <- renderText({
    dex()
  })

}

shinyApp(ui = ui, server = server)
