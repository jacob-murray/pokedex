library(dplyr)

species_df <- tibble(
  id = NA,
  color  = NA,
  dex_text = NA,
  habitat = NA)

for(i in 1) {
  
  print(i)
  print(glue::glue("https://pokeapi.co/api/v2/pokemon-species/{i}"))
  
  species <- jsonlite::fromJSON(glue::glue("https://pokeapi.co/api/v2/pokemon-species/{i}"), flatten = TRUE)
  
  id <- species$id
  print(id)
  color <- species$color$name
  print(color)
  dex_text <- species$flavor_text_entries$flavor_text |>
    tibble::as_tibble() |>
    mutate(language = textcat::textcat(value)) |>
    filter(language == "english") |>
    slice_head(n = 1) |>
    pull(value) |>
    stringr::str_replace_all("\n|\f", " ")
  print(dex_text)
  habitat <- species$habitat$name
  print(habitat)
  
  species_df <- add_row(species_df, 
                        id = id,
                        color  = color,
                        dex_text = dex_text,
                        habitat = habitat)
}

species_df_final <- species_df |>
  filter(!is.na(id)) |>
  readr::write_csv("data/dictionaries/species_dictionary.csv")
