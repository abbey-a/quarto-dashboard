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
race_demo <- covid_clean |> 
  group_by(case_race) |> 
  summarize(Count=n()) |> 
  mutate(case_race = str_to_title(ifelse(is.na(case_race), "MISSING", case_race))) |> 
  arrange(Count) 

# Age, Sex
covid_cases_demo_base <- covid_clean |>
  mutate(
    # age group (tweak bins if you want)
    age_group = case_when(
      is.na(case_age) ~ "Missing",
      case_age < 0 ~ "Missing",
      case_age <= 17 ~ "0–17",
      case_age <= 29 ~ "18–29",
      case_age <= 44 ~ "30–44",
      case_age <= 64 ~ "45–64",
      case_age >= 65 ~ "65+",
      TRUE ~ "Missing"
    ),
    age_group = factor(age_group, levels = c("0–17","18–29","30–44","45–64","65+","Missing")),
    
    # sex (normalize common values)
    sex = case_when(
      is.na(case_gender) ~ "Missing",
      str_to_upper(case_gender) %in% c("M", "MALE") ~ "Male",
      str_to_upper(case_gender) %in% c("F", "FEMALE") ~ "Female",
      TRUE ~ "Other/Unknown"
    ),
    sex = factor(sex, levels = c("Female","Male","Other/Unknown","Missing"))
  )

# pivot wide for stacked bar
cases_age_sex_wide <- covid_cases_demo_base |>
  count(age_group, sex, name = "cases") |>
  tidyr::pivot_wider(
    names_from  = sex,
    values_from = cases,
    values_fill = 0
  ) |>
  arrange(age_group)


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



