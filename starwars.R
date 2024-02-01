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


# HW 11

library(ggplot2)
library(dplyr)

starwars_data <- read.csv('sw-wrangled.csv')

# Histogram of the height_cm column
ggplot(starwars_data, aes(x = height_cm)) +
  geom_histogram(binwidth = 10, fill = "grey", color = "black") +
  labs(x = "height_cm", y = "count")


# Histogram of hair colors sorted
library(ggplot2)
library(dplyr)

# Histogram of hair color sorted
starwars_data <- starwars_data %>% 
  filter(!is.na(hair)) 
starwars_data %>%
  count(hair) %>% 
  mutate(sorted_hair = reorder(hair, n)) %>%
  ggplot(aes(x = sorted_hair, y = n)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  labs(x = "Hair Color", y = "Count")

# Scatter plot of mass vs. height_in
ggplot(starwars_data, aes(x = height_in, y = mass)) +
  geom_point(shape = 17) +  # shape 17 is a filled triangle
  labs(x = "height_in", y = "mass", ylim = c(0,50))

# The outlier (1358) ? How should I manage it?