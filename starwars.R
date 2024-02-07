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


# HW 13 (1)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggsci)
library(extrafont)

starwars_data <- read.csv('sw-wrangled.csv')
starwars_data <- starwars_data %>%
  filter(mass != 1358) %>%
  mutate(gender = fct_recode(gender,
                           "Female" = "f",
                           "Male" = "m")) # How to convert NA values into "others?"

p <- ggplot(starwars_data, aes(x = height_cm, y = mass)) +
  geom_point(aes(color = gender), alpha = 0.5) + 
  geom_smooth(aes(color = gender), method = "lm", se = FALSE, alpha = 0.3) + 
  facet_wrap(~gender, scales = "free_y") +
 theme_minimal(base_family = "Arial") +
  labs(
    title = "Height and weight across gender presentation",
    subtitle = "A cautionary tale in misleading 'free' axis scales & bad design choices",
    x = "Height (cm)",
    y = "Mass (kg)",
    color = "Gender Presentation"
  ) +
  theme(
    plot.title = element_text(family = "Comic Sans MS", size = 10, hjust = 0, face = "bold"),
    plot.subtitle = element_text(family = "Comic Sans MS", size = 6, hjust = 0, face = "italic"),
    strip.text = element_text(size = 6, hjust = 0, face = "bold", color = "white"),
    strip.background = element_rect(fill = "darkgreen"),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 6, face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "thistle", color = NA), # How to reduce legend box size?
    legend.title = element_text(family = "Times New Roman"), # Which font is that? How to change NA value color to orange?
    panel.background = element_rect(fill = "mistyrose1"),
    panel.grid.major = element_line(color = "lightgrey"),  # How to make dotted grid lines?
    panel.grid.minor = element_blank(), # How to add range to each line?
    axis.text.x = element_text(angle = 45, hjust = 1))
print(p)
  

# HW 13 (2) Modifying HW 11 plot

starwars_data <- read.csv('sw-wrangled.csv') %>%
  filter(!is.na(hair)) %>%
  count(hair) %>%
  mutate(sorted_hair = reorder(hair, n))

ggplot(starwars_data, aes(x = sorted_hair, y = n)) +
  geom_bar(stat = "identity", fill = "pink3", color = "black") + # Colors changed for aesthetics
  labs(x = "Hair Color", y = "Count", title = "Distribution of Hair Colors") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#2F4F4F"), # Color of x axis texts
        axis.title.x = element_text(face = "bold", color = "#2F4F4F"), # Color and bold x axis title
        axis.title.y = element_text(face = "bold", color = "#2F4F4F"), # Color and bold y axis title
        plot.title = element_text(hjust = 0.5, face = "bold", color = "black")) # Centered and colored plot title

# HW 13 (2) Modifying HW 12 plot

starwars_data <- read.csv('sw-wrangled.csv')
data <- starwars_data %>%
  filter(mass != 1358) %>%
  mutate(brown_hair_category = ifelse(hair == "brown", "Has brown hair", "No brown hair"))

ggplot(data, aes(x = mass, y = height_in)) +
  geom_point(aes(color = brown_hair_category), alpha = 0.6) + # Added color aesthetic and adjusted alpha for points
  geom_smooth(method = "lm", aes(fill = brown_hair_category), color = "black", alpha = 0.2) + # Added fill aesthetic for smooth
  scale_color_manual(values = c("Has brown hair" = "#A52A2A", "No brown hair" = "#A9A9A9")) + # Custom colors for points
  scale_fill_manual(values = c("Has brown hair" = "#A52A2A", "No brown hair" = "#A9A9A9")) + # Custom colors for smooth fill
  facet_wrap(~brown_hair_category) +
  labs(title = "Mass vs. Height by Brown Hair Category",
       subtitle = "A critically important analysis",
       x = "Mass (kg)", y = "Height (in)",
       caption = "Data source: Star Wars API") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "bottom", # Move legend to bottom
        legend.title = element_blank(), # Remove legend title
        plot.caption = element_text(hjust = 0, face = "italic"))

# ```{r image-chunk-name, fig.cap="This is the caption for the image."}
knitr::include_graphics("image/bird.jpg")
