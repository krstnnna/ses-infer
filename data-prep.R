library(readxl)
library(dplyr)
library(writexl)
library(tidyverse)

# Read data
dataset <- read_xlsx("dataset.xlsx")

# Tidiness check
str(dataset)

# Data wrangling

data_prep <- dataset %>%
  select(type, gender, age, edu, class, hire, a_inc, a_edu, a_class, comp, intel) %>%   # select columns
  filter(age > 35) %>%    # only include participants above age 35
  mutate(across(c(type, gender), factor)) %>%
  mutate(type = case_when(
    type == "1" ~ "upper-class",
    type == "2" ~ "lower-class",
    TRUE ~ type  )) %>%   # name two types of stimuli
  mutate(gender = case_when(
    gender == "1" ~ "male",
    gender == "2" ~ "female",
    TRUE ~ gender  )) %>%   # name two types of gender
  mutate(across(c(type, gender), factor)) %>%   # type and gender are factor variables
  mutate(social_class = type, evaluator_gender = gender, evaluator_age = age, evaluator_edu = edu,
         evaluator_class = class, hiring_pref = hire, perceived_inc = a_inc, perceived_edu = a_edu,
         perceived_class = a_class, perceived_comp = comp, perceived_intel = intel) %>%   # change names
  select(social_class, perceived_inc, perceived_edu, perceived_class, perceived_comp, perceived_intel, hiring_pref,
         evaluator_gender, evaluator_age, evaluator_edu, evaluator_class)   # final column selection

# Study & data description:
# Participants read one of the two personal narratives and indicate their perceptions of the narrator.
# perceived_(  ) refers to participants' perception of the narrator. evaluator_(  ) refers to participants' demographic info.

# Tidiness check
str(data_prep)

# Export data
write_xlsx(data_prep, "data_prep.xlsx")

