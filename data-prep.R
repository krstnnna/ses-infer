# Read data
library(readxl)
data <- read_xlsx("dataset.xlsx")

# Tidiness check
str(data)
library(dplyr)
library(writexl)

# Data wrangling
# 1. Select columns and filter data
data_prep <- data %>%
  select(type, age, edu, class, rich, hire, a_inc, a_edu, a_class, comp, warm) %>%
  filter(age > 35)

# 2. Convert data types
data_prep$type <- as.factor(data_prep$type)

# Tidiness check
str(data_prep)

# Export data
write_xlsx(data_prep, "data_prep.xlsx")

# Need to figure out how to handle missing data
