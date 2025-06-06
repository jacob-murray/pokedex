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
      textOutput("pokemon"),
    ),
    card_body(
      textOutput("id")
    )
  ),
  card(
    card_header(
      textOutput("type_color_primary")
    ),
    card_body(
      tableOutput("base_stats")
    )
  ),
  card(
    card_header(
      textOutput("type_color_secondary")
    ),
    card_body(
      plotOutput("base_stats_plot")
    )
  )
)

server <- function(input, output, session){
  
  dex <- reactive({
    req(input$pokemon_name)
    dex <- jsonlite::fromJSON(glue::glue("data/{stringr::str_to_lower(input$pokemon_name)}.json"),flatten=TRUE)
  })

  pokemon_type_primary <- reactive({
    dex()$types$type.name[1] |>
      as.character()
  })

  pokemon_type_secondary <- reactive({
    # creating types
    if(!is.na(dex()$types$type.name[2])){
      print('secondary type exists')
      dex()$types$type.name[2] |>
        as.character()
    } else{
      print('secondary type does not exist')
      NA
    }
  })

  type_color_primary <- reactive({
    color_dictionary[
      dex()$types$type.name[1] |>
      as.character()]
  })

  type_color_secondary <- reactive({
    
    if(!is.na(dex()$types$type.name[2])){
    color_dictionary[
      dex()$types$type.name[2] |>
      as.character()]
    } else {
      "#D3D3D3"
    }
  })

  output$pokemon <- renderText({
    input$pokemon_name
  })

  output$id <- renderText({
    glue::glue('Pokedex Entry #{
    dex()$id |> 
      as.character()
    }
    ')
  })

  output$base_stats <- renderTable({
    dex()$stats |>
      tibble::as_tibble() |>
      select(base_stat, stat.name ) |>
      rename(stat_name = stat.name) |>
      tidyr::pivot_wider(
        names_from = stat_name, values_from = base_stat
      )
  })

  output$base_stats_plot <- renderPlot({

    max_stat <- dex()$stats |>
      summarize(max = max(base_stat, na.rm = T))

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
                       linewidth = 18
                       ) +
      scale_colour_identity() +
      geom_point(size=18, color=type_color_secondary()) +
      theme_bw() +
      theme(text = element_text(size = 25),
            axis.title.y=element_blank(),
            axis.title.x.bottom = element_blank(),
            axis.ticks.x=element_blank()) + 
      scale_x_discrete(guide = guide_axis(angle = 75)) +
      ylim(0, max(max_stat) * 1.1)
  })

}

shinyApp(ui = ui, server = server)
