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

# HW 12

library(dplyr)
library(ggplot2)
starwars_data <- read.csv('sw-wrangled.csv')
data <- starwars_data %>%
  filter(mass != 1358)

# Plot 1
plot <- ggplot(data, aes(x = hair, y = mass, fill = hair)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  labs(x = "Hair color (s)", y = "Mass (kg)")
print(plot)

# Plot 2

data$brown_hair_category <- ifelse(data$hair == "brown", "Has brown hair", "No brown hair")
plot <- ggplot(data, aes(x = mass, y = height_in)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "blue", fill = "blue", alpha = 0.2) +
  facet_wrap(~brown_hair_category) +
  labs(title = "Mass vs. height by brown-hair-havingness",
       subtitle = "A critically important analysis",
       x = "mass", y = "height_in") +
       theme_minimal()
print(plot)

# Plot 3

species_gender_count <- data %>%
  group_by(species_first_letter = substr(species, 1, 1), gender) %>%
  summarise(count = n(), .groups = 'drop')
plot <- ggplot(species_gender_count, aes(x = species_first_letter, y = count, fill = gender)) +
  geom_bar(stat = 'identity', position = 'stack') +
  coord_flip() +
  labs(x = "species first letter", y = "count", title = "A clear male human bias") +
  theme(plot.title = element_text(hjust = 1, size = 8, vjust = 1)) +
  theme(legend.position="bottom") +
  scale_fill_manual(values = c("m" = "cyan3", "f" = "salmon")) +
  theme_minimal()
plot <- plot + 
  theme(plot.title = element_text(hjust = 1, vjust = 1, size = 10, face = "plain"),
        plot.title.position = "plot")
print(plot)
