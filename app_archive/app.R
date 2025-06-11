# libraries
library(dplyr)
library(ggplot2)
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

base_stat_order <- c('hp', 'attack', 'defense', 'speed',
                     'sp attack', 'sp defense')

evolutions <- readr::read_csv("data/dictionaries/evolutions_dictionary.csv") |>
  mutate(pokemon = stringr::str_to_title(pokemon))

dex_dictionary <- readr::read_csv("data/dictionaries/dex_dictionary.csv") |>
  filter(!is.na(first_game))

# pokemon_caught (filtering for missing newer pokemon)
pokemon_caught <- evolutions |>
  filter(pokemon %in% stringr::str_to_title(dex_dictionary$pokemon_name))

# joining df
dex_dictionary_joined <- dex_dictionary |>
  mutate(pokemon_name = stringr::str_to_title(pokemon_name)) |>
  left_join(evolutions, by = c("pokemon_name" = "pokemon"))

first_evolutions <- pokemon_caught |>
  filter(evolution == "first") |>
  pull(pokemon)

# generations available
generations <- dex_dictionary |>
  pull(generation) |>
  unique()

# games available 
games <- dex_dictionary |>
  pull(first_game) |>
  unique()
  
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


# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "Pokedex",
  sidebar = sidebar(
    selectInput(
      "generation", "Generation",
      choices = generations
    ),
    uiOutput("starters_list"),
    
    
    
    selectInput(
      "pokemon_starter", "Select a Species",
      choices = first_evolutions
    ),
    uiOutput("selected_pokemon")
  ),
  plotOutput("base_stats_plot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe({
    updateSelectInput(
      session, "starter_test",
      choices = dex_dictionary |>
        distinct(generation, pokemon_name) |>
        filter(generation == input$generation) |>
        left_join(evolutions, by = c(str::str_to_title(pokemon_name) = pokemon)) |>
        pull(name)
    )
  })
  
  
  # filtering starter pokemon based on generation
  starters <- reactive({
    req(input$generation)
    dex_dictionary |>
      filter(generation == input$generation) |>
      left_join(evolutions, by = c(str::str_to_title(pokemon_name) = Pokemon)) |>
      filter(evolution == "first") |>
      pull(pokemon_name) |>
      stringr::str_to_title() |>
      unique()
  })
  
  # selecting pokemon based on starter
  output$starters_list <- renderUI({
    req(input$generation)
    selectInput("selected_pokemon", 
                "Select a Pokemon", 
                choices = starters()
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  # evolution entry
  species <- reactive({
    req(input$pokemon_starter)
    
    selected_species_id <- evolutions |>
      filter(pokemon == input$pokemon_starter) |>
      pull(species_id) |>
      unique()
    
    evolutions |>
      filter(species_id == selected_species_id) |>
      pull(pokemon) |>
      unique()
    
  })
  
  # selecting pokemon based on starter
  output$selected_pokemon <- renderUI({
    req(input$pokemon_starter)
    selectInput("selected_pokemon", 
                "Select a Pokemon", 
                choices = species()
                )
  })
  
  # pokemon name
  output$pokemon <- renderText({
    req(input$pokemon_name)
    input$pokemon_name
  })
  
  # getting dex entry for selected pokemon
  dex <- reactive({
    
    req(input$selected_pokemon)
    dex <- jsonlite::fromJSON(glue::glue("data/dex/{input$selected_pokemon}.json"),flatten=TRUE)
    
  })
  
  # primary type
  pokemon_type_primary <- reactive({
    dex()$types$type.name[1] |>
      as.character()
  })
  
  # secondary type
  pokemon_type_secondary <- reactive({
    if(!is.na(dex()$types$type.name[2])){
      print('secondary type exists')
      dex()$types$type.name[2] |>
        as.character()
    } else{
      print('secondary type does not exist')
      NA
    }
  })
  
  # primary type color
  type_color_primary <- reactive({
    color_dictionary[
      dex()$types$type.name[1] |>
        as.character()]
  })
  
  # primary type color: static
  output$type_color_primary_static <- renderText({
    color_dictionary[
      dex()$types$type.name[1] |>
        as.character()]
  })
  
  # secondary type color
  type_color_secondary <- reactive({
    
    if(!is.na(dex()$types$type.name[2])){
      color_dictionary[
        dex()$types$type.name[2] |>
          as.character()]
    } else {
      "#D3D3D3"
    }
  })
  
  # secondary type color: static
  output$type_color_secondary_static <- renderText({
    if(!is.na(dex()$types$type.name[2])){
      color_dictionary[
        dex()$types$type.name[2] |>
          as.character()]
    } else {
      "#D3D3D3"
    }
  })
  
  # base stats plot
  output$base_stats_plot <- renderPlot({
    
    dex()$stats |>
      tibble::as_tibble() |>
      select(base_stat, stat.name ) |>
      rename(stat_name = stat.name) |>
      mutate(stat_name = case_when(stat_name == "special-attack" ~ 'sp attack',
                                   stat_name == "special-defense" ~ 'sp defense',
                                   .default = stat_name)) |>
      mutate(stat_name = factor(stat_name, levels = base_stat_order)) %>%
      ggplot(aes(x=stat_name, y=base_stat)) +
      geom_segment(aes(xend=stat_name, yend=0, color=type_color_primary()), 
                   linewidth = 16
      ) +
      scale_colour_identity() +
      geom_point(size=16, color=type_color_secondary()) +
      theme_bw() +
      theme(text = element_text(size = 25),
            axis.title.y=element_blank(),
            axis.title.x.bottom = element_blank(),
            axis.ticks.x=element_blank()) + 
      scale_x_discrete(guide = guide_axis(angle = 75)) +
      ylim(0, 150)
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
