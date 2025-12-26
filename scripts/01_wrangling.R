# Foundational Wrangling for Visuals

# 1. Epi Curve Overview
# weekly cases
weekly_overview <- covid_clean |>
  filter(!is.na(pos_sampledt)) |> # n=122 missing positive sample dates
  mutate(week_start = lubridate::floor_date(pos_sampledt, "weeks", week_start = 1)) |>
  count(week_start, name = "Number of Cases") |> 
  mutate(cdc_week = epiweek(week_start)) |> 
  mutate(label = paste0("CDC Week ", cdc_week))

# 2. Race/Demographics
cases_demo <- covid_clean |> 
  group_by(case_race) |> 
  summarize(Count=n()) |> 
  mutate(case_race = str_to_title(ifelse(is.na(case_race), "MISSING", case_race))) |> 
  arrange(Count) 

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