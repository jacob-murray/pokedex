library(dplyr)

evolution_df <- tibble(
  species_id  = NA,
  first_evolution = NA,
  second_evolution = NA,
  third_evolution = NA)

for(i in 1) {
  print(i)
  
  evolution <- jsonlite::fromJSON(glue::glue("https://pokeapi.co/api/v2/evolution-chain/{i}/"), flatten = TRUE)
  
  species_id <- evolution$id
  
  first_evolution <- evolution$chain$species$name
  
  second_evolution <- evolution$chain$evolves_to$species.name
  
  if(nrow(
    evolution$chain$evolves_to$evolves_to |>
    as.data.frame() 
  ) > 0 ){
    
    third_evolution <- evolution$chain$evolves_to$evolves_to |>
      as.data.frame() |>
      pull(species.name)
    
  } else {
    third_evolution <- NA
  }
  
  evolution_df <- add_row(evolution_df, 
                          species_id = species_id,
                          first_evolution = first_evolution,
                          second_evolution = second_evolution,
                          third_evolution = third_evolution)
  
}

evolution_df_final <- evolution_df |>
  rename(first = first_evolution,
         second = second_evolution,
         third = third_evolution) |>
  tidyr::pivot_longer(!species_id,
                      names_to = 'evolution',
                      values_to = 'pokemon') |>
  filter(!is.na(species_id)) |>
  filter(!is.na(pokemon))

readr::write_csv(evolution_df_final, "data/evolution/evolutions.csv")

