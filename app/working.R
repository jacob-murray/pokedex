
library(dplyr)
library(ggplot2)

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

pokemon_name <- "totodile"

dex <- jsonlite::fromJSON(glue::glue("data/{stringr::str_to_lower(pokemon_name)}.json"),flatten=TRUE)

base_stat_order <- c('hp', 'attack', 'defense', 'speed',
                     'sp attack', 'sp defense')

pokemon_type_primary <- dex$types$type.name[1]

# creating types
if(!is.na(dex$types$type.name[2])){
  print('secondary type exists')
  pokemon_type_secondary <- dex$types$type.name[2]
} else{
  print('secondary type does not exist')
  pokemon_type_secondary <- NA
}

# getting type color
type_color_primary <- as.character(color_dictionary[pokemon_type_primary])
if(!is.na(pokemon_type_secondary)){
  print('secondary color exists')
  type_color_secondary <- as.character(color_dictionary[pokemon_type_secondary])
} else{
  print('secondary color does not exist')
  type_color_secondary <- "grey"
}

dex$stats |>
  tibble::as_tibble() |>
  select(base_stat, stat.name ) |>
  rename(stat_name = stat.name) |>
  mutate(stat_name = case_when(stat_name == "special-attack" ~ 'sp attack',
                               stat_name == "special-defense" ~ 'sp defense',
                              .default = stat_name)) |>
  mutate(stat_name = factor(stat_name, levels = base_stat_order)) %>%
  ggplot(aes(x=stat_name, y=base_stat)) +
  geom_segment(aes(xend=stat_name, yend=0, color= type_color_primary), 
                   linewidth = 4.5
                   ) +
  scale_colour_identity() +
  geom_point(size=4, color=type_color_secondary) +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.ticks.x=element_blank()) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
