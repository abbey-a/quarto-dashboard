# Foundational Wrangling for Visuals

# 1. Epi Curve Overview
# weekly cases (and hospitalizations variable for later)
weekly_overview <- covid_clean |>
  filter(!is.na(pos_sampledt)) |> # n=122 missing positive sample dates
  mutate(week_start = lubridate::floor_date(pos_sampledt, "weeks", week_start = 1)) |>
  group_by(week_start) |>
  summarize(
    `Number of Cases` = dplyr::n(),
    `Number Hospitalized` = sum(hospitalized == "Yes", na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    hosp_rate = `Number Hospitalized` / `Number of Cases`,
    cdc_week  = epiweek(week_start),
    label     = paste0("CDC Week ", cdc_week)
  )


# 2. Cases 
# Race/Demographics
cases_demo <- covid_clean |> 
  group_by(case_race) |> 
  summarize(Count=n()) |> 
  mutate(case_race = str_to_title(ifelse(is.na(case_race), "MISSING", case_race))) |> 
  arrange(Count) 

# Weekly Hospitalization Rate Among Reported Cases



# 3. Deaths
covid_deaths <- covid_clean |> 
  filter(died_covid == "Yes") |> 
  filter(!is.na(pos_sampledt)) |>
  mutate(week_start = lubridate::floor_date(pos_sampledt, "weeks", week_start = 1)) |>
  count(week_start, name = "Number of Deaths")

weekly_overview <- weekly_overview |>
  left_join(covid_deaths, by = "week_start") |>
  mutate(
    # replace NA with 0
    `Number of Deaths` = tidyr::replace_na(`Number of Deaths`, 0L),
    death_rate = `Number of Deaths` / `Number of Cases`)