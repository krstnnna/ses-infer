library(tidyverse)
library(dplyr)
library(tibble)
starwars <- dplyr::starwars

sw.wrangled <- starwars %>%
  separate(name, into = c("first_name", "last_name"), sep = " ") %>%
  mutate(height_in = height * 0.393701, height_cm = height, hair = hair_color, brown_hair = hair_color == "brown") %>%
  select(-hair_color) %>%
  mutate(gender = case_when(
    gender == "masculine" ~ "m",
    gender == "feminine" ~ "f",
    TRUE ~ gender  )) %>%
  mutate(across(c(mass, hair, gender, homeworld), factor)) %>%
  select(first_name, last_name, height_in, height_cm, mass, hair, gender, species, homeworld, brown_hair)
  
sw.wrangled

# Am I using the right built-in starwars dataset?
# How do I import the the goal df to my data repo for comparison?