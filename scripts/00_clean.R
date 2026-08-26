pacman::p_load(tidyverse, readxl, janitor, stringr, lubridate)

# Read and standardize the synthetic line list.
covid_raw <- read_excel("data/covid_example_data.xlsx")

covid_clean <- covid_raw |>
  clean_names() |>
  rename_with(~ str_remove(.x, "_false$")) |>
  distinct()

# Fourteen repeated PIDs have matching clinical/date fields and differ only in
# synthetic geography. Retain one deterministic record per case ID.
duplicate_pid_rows <- sum(duplicated(covid_clean$pid) & !is.na(covid_clean$pid))

covid_clean <- covid_clean |>
  arrange(pid, case_zip, latitude_jitt, longitude_jitt) |>
  distinct(pid, .keep_all = TRUE)
