# libraries
library(dplyr)
library(ggplot2)
library(jsonlite)
library(shiny)
library(bslib)


# getting font for ggplot object
library(showtext)
sysfonts::font_add_google("Play", family = "special")
showtext_auto()

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

# creating order of base stats for plots
base_stat_order <- c('hp', 'attack', 'defense', 'speed',
                     'sp attack', 'sp defense')

# pokemon df
pokemon_caught <- readr::read_csv("data/dictionaries/dex_dictionary.csv") |>
  filter(!is.na(first_game)) |>
  inner_join(readr::read_csv("data/dictionaries/evolutions_dictionary.csv"), 
             by=c("pokemon_name"="pokemon")) |>
  inner_join(readr::read_csv("data/dictionaries/species_dictionary.csv"),
             by = "id") |>
  mutate(pokemon_name = stringr::str_to_title(pokemon_name))

# dictionary for first evolution pokemon
first_evolutions_dictionary <- pokemon_caught |>
  filter(evolution == "first") |>
  pull(local_url, pokemon_name)

# generations available
generations <- pokemon_caught |>
  pull(generation) |>
  unique()

# games available 
games <- pokemon_caught |>
  pull(first_game) |>
  unique()

# define ui
ui <- page_sidebar(
  title = "Pokedex",
  theme = bs_theme(heading_font = font_google("Press Start 2P"),
                   base_font = font_google("Play")),
  sidebar = sidebar(
    selectInput(
      "generation", "Generation",
      choices = generations,
    ),
    selectInput(
      "first_gen", "Select a Starter",
      choices = c()
    ),
    selectInput(
      "pokemon_selected", "Select a Pokemon",
      choices = c()
    ),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    tags$style(HTML("
    .card {
    padding: 0px;
    gap: 0px !important;
    box-shadow: rgba(0, 0, 0, 0.24) 0px 3px 8px;
    }")),
    tags$style(".shiny-image-output {
    display: flex;
    justify-content: center;
    height: 100px !important;
    padding: 0px !important;
    gap: 0px !important;
    margin: 0px !important;
               }"),
    card(
      card_header("Evolutions"),
      card_body(uiOutput("sprites"))
    )
  ),
  # hiding red warning on json read
  layout_columns(
    card(
      card_header("Base Stats"),
      card_body(plotOutput("base_stats_plot"))
    ),
    layout_columns(
      card(
        card_header("Selected Pokemon"),
        card_body(uiOutput("selected_sprite"))
      ),
      card(
        card_header("First Game Apperance"),
        card_body(uiOutput("first_game"))
      ),
      card(
        card_header("Pokedex Entry"),
        card_body(
          textOutput("dex_entry")
        )
      ),
      col_widths = c(6,6, 12),
      row_heights = 6
    ),
    col_widths = c(6, 6)
  )
)


# server
server <- function(input, output, session) {
  
  pokemon <- reactive({
    req(input$generation)
    readr::read_csv("data/dictionaries/dex_dictionary.csv") |>
      filter(!is.na(first_game)) |>
      inner_join(readr::read_csv("data/dictionaries/evolutions_dictionary.csv"), 
                 by=c("pokemon_name"="pokemon")) |>
      inner_join(readr::read_csv("data/dictionaries/species_dictionary.csv"),
                 by = "id") |>
      mutate(pokemon_name = stringr::str_to_title(pokemon_name)) |>
      filter(generation == input$generation)
  })
  
  # selector for first generation pokemon -- based on generation
  observe({
    updateSelectInput(
      session, "generation_test",
      choices = pokemon() |>
        distinct(generation) |>
        pull(generation) 
    )
  })
  
  # selector for first generation pokemon -- based on generation
  observe({
    updateSelectInput(
      session, "first_gen",
      choices = pokemon() |>
        distinct(generation, evolution, pokemon_name) |>
        filter(evolution == "first") |>
        pull(pokemon_name) 
    )
  })
  
  # getting species id
  species_id <- reactive({
    req(input$first_gen)
    pokemon() |>
      filter(pokemon_name == input$first_gen) |>
      pull(species_id) |>
      unique()
  })
  
  # listing pokemon by species
  species_list <- reactive({
    req(species_id())
    pokemon() |>
      distinct(generation, evolution, species_id, pokemon_name) |>
      filter(species_id == species_id()) |>
      pull(pokemon_name)
  })
  
  # select a species -- based on gen and starter
  observe({
    req(species_id())
    updateSelectInput(
      session, "pokemon_selected",
      choices = species_list(),
      selected = NULL
    )
  })
  
  # getting dex entry for selected pokemon
  dex <- reactive({
    req(input$pokemon_selected)
    pokemon() |>
      filter(pokemon_name == input$pokemon_selected) |>
      pull(local_url) |>
      unique() |>
      jsonlite::fromJSON()
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
  
  
  # generates dex ids for all pokemon in evolution line
  evolution_line_ids <- reactive({
    req(species_id())
    pokemon() |>
      distinct(species_id, id) |>
      filter(species_id == species_id()) |>
      pull(id)
  })
  
  # evolution sprites
  output$sprites <- renderUI({
    
    # creating list to house reactive pngs
    sprite_pngs <- list()
    
    # adding file names to list
    for(i in 1:length(evolution_line_ids())){
      print(evolution_line_ids()[i])
      file <- normalizePath(
        file.path(
          glue::glue("./sprites/pokemon/front/{evolution_line_ids()[i]}.png")
        ))
      sprite_pngs <- append(sprite_pngs, file)
    }
    
    sprite_pngs %>% purrr::map(
      function(file){
        renderImage(
          list(
            src = file, 
            alt = "Pokemon", 
            height = "100%"
          )
          , deleteFile = FALSE)
      }
    )
  })
  
  
  # selected_sprite sprite
  output$selected_sprite <- renderUI({
    
    sprite_selected_pngs <- list()
    
    filename_front <- normalizePath(
      file.path(glue::glue("./sprites/pokemon/front/{dex()$id |> as.character()}.png")))
    
    
    sprite_selected_pngs <- append(sprite_selected_pngs, filename_front)
    
    filename_back <- normalizePath(
      file.path(glue::glue("./sprites/pokemon/back/{dex()$id |> as.character()}.png")))
    
    sprite_selected_pngs <- append(sprite_selected_pngs, filename_back)
    
    layout_columns(
      sprite_selected_pngs %>% purrr::map(
        function(file){
          renderImage(
            list(
              src = file, 
              alt = "Pokemon", 
              height = "100%"
            )
            , deleteFile = FALSE)
        }
      ),
      col_widths = c(-2,8,-2)
    )
    
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
      theme_classic() +
      theme(panel.background = element_rect(fill = "darkgrey"),
            text = element_text(size = 25),
            axis.title.y=element_blank(),
            axis.title.x.bottom = element_blank(),
            axis.ticks.x=element_blank()) + 
      scale_x_discrete(guide = guide_axis(angle = 75)) +
      ylim(0, 150)
  })
  
  # first game appearance
  output$first_game <- renderUI({
    
    filename_game <- normalizePath(
      file.path(glue::glue("./images/game/{dex()$game_indices$version.name[1]}.png")))
    
    renderImage(
      list(
        src = filename_game, 
        alt = "first game appearance", 
        height = "100%"
      ), deleteFile = FALSE)
  })
  
  # dex entry
  output$dex_entry <- renderText({
    text <- pokemon() |>
      filter(pokemon_name == input$pokemon_selected) |>
      pull(dex_text) |>
      unique()
  })
  
  
}

# run the application 
shinyApp(ui = ui, server = server)
