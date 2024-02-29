library(readxl)
library(dplyr)
library(writexl)
library(tidyverse)

# Read data
dataset <- read_xlsx("dataset.xlsx")

# Tidiness check
str(dataset)
vignette_type$dataset -> as.factor(vignette_type$dataset)

# Data wrangling

data_prep <- dataset %>%
  select(vignette_type, gender, age, edu, class, knowledge, topic_knowledge, a_inc, a_edu, a_class) %>%   # select columns
  filter(age > 35) %>%    # only include participants above age 35
  mutate(across(c(vignette_type, gender), factor)) %>%
  mutate(vignette_type = case_when(
    vignette_type == "1" ~ "higher-class",
    vignette_type == "2" ~ "lower-class",
    TRUE ~ vignette_type  )) %>%   # name two types of stimuli
  mutate(gender = case_when(
    gender == "1" ~ "male",
    gender == "2" ~ "female",
    TRUE ~ gender  )) %>%   # name two types of gender
  mutate(across(c(vignette_type, gender), factor)) %>%   # type and gender are factor variables
  mutate(evaluator_gender = gender, evaluator_age = age, evaluator_edu = edu,
         evaluator_class = class, infer_knowledge = knowledge, perceived_inc = a_inc, perceived_edu = a_edu,
         perceived_class = a_class) %>%   # change names
  select(vignette_type, perceived_inc, perceived_edu, perceived_class, infer_knowledge, topic_knowledge,
         evaluator_gender, evaluator_age, evaluator_edu, evaluator_class)   # final column selection

# Study & data description:
# Participants read one of the two (higher-class and lower-class) vignettes of conversation and indicate their inferences of the listener.
# perceived_(  ) refers to participants' perception of the narrator. evaluator_(  ) refers to participants' demographic info.

# Tidiness check
str(data_prep)

# Export data
write_xlsx(data_prep, "data_prep.xlsx")

