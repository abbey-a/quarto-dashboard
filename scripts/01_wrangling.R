# Foundational Wrangling for Visuals

# 1. Epi Curve Overview
# weekly cases
grouped_overview <- covid_clean |>
  filter(!is.na(pos_sampledt)) |> # n=122 missing positive sample dates
  mutate(week_start = lubridate::floor_date(pos_sampledt, "weeks", week_start = 1)) |>
  count(week_start, name = "Number of Cases")

grouped_overview$cdc_week <- epiweek(grouped_overview$week_start)
grouped_overview$label = paste0("CDC Week ", grouped_overview$cdc_week)

# 2. Cases
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