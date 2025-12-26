pacman::p_load(tidyverse, readxl, janitor, stringr)

# read in data
covid_raw <- read_excel("data/covid_example_data.xlsx")

# tidy column names with janitor
covid_clean <- clean_names(covid_raw)

# deduplicate any identical rows
covid_clean <- covid_clean[!duplicated(covid_clean),] # same # rows, no duplicates

# drop "_false" endings from column names
colnames_clean <- gsub("_false", "", colnames(covid_clean))
colnames(covid_clean) <- colnames_clean