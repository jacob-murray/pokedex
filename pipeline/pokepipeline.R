# empty dex dictionary
dex_df <- tibble(
  id  = NA,
  pokemon_name = NA,
  pokemon_name_str = NA,
  local_url = NA,
  first_game = NA
  )

for(i in 1:1){
  
  print(i)
  pokemon_id <- i
  url <- glue::glue('https://pokeapi.co/api/v2/pokemon/{i}')
  result <- httr::GET(url)
  data <- jsonlite::fromJSON(httr::content(result,"text"),flatten=TRUE)
  pokemon_name <- data$species$name
  pokemon_name_str <- stringr::str_replace(pokemon_name, "-", "_")
  jsonlite::write_json(data, glue::glue("data/dex/{pokemon_name_str}.json"))
  pokemon_id = data$id
  first_game = data$game_indices$version.name[1]
  
  dex_df <- add_row(dex_df, 
                    id = pokemon_id,
                    pokemon_name = pokemon_name,
                    pokemon_name_str = pokemon_name_str,
                    local_url = glue::glue("data/dex/{pokemon_name_str}.json"),
                    first_game = first_game)
  
  Sys.sleep(time = 1)
  
}

dex_df_final <- dex_df |>
  filter(!is.na(id))

readr::write_csv(dex_df_final, "data/dex_dictionary.csv")

