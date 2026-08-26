# Foundational wrangling for the dashboard

normalise_binary <- function(x) {
  case_when(
    is.na(x) | str_trim(x) == "" ~ NA_character_,
    str_to_lower(str_trim(x)) == "yes" ~ "Yes",
    str_to_lower(str_trim(x)) == "no" ~ "No",
    TRUE ~ str_trim(x)
  )
}

safe_pct_change <- function(current, previous) {
  if_else(!is.na(previous) & previous != 0,
          100 * (current - previous) / previous,
          NA_real_)
}

format_wow <- function(current, previous) {
  change <- current - previous

  if (is.na(previous)) {
    return("Prior week unavailable")
  }
  if (previous == 0) {
    if (current == 0) return("No change")
    return(paste0(ifelse(change > 0, "+", ""), change, " vs prior week"))
  }

  change_pct <- 100 * change / previous
  if (change_pct == 0) return("No change")

  direction <- ifelse(change_pct > 0, "↑ ", "↓ ")
  paste0(direction, sprintf("%.0f%% vs prior week", abs(change_pct)))
}

format_kpi_change <- function(current, previous) {
  change <- current - previous
  if (is.na(previous)) return("NA")
  if (previous == 0) return(ifelse(change == 0, "0", sprintf("%+d", change)))
  sprintf("%+.1f%%", 100 * change / previous)
}

age_group_levels <- c("0-17", "18-29", "30-44", "45-64", "65+")

covid_analysis <- covid_clean |>
  mutate(
    hospitalized = normalise_binary(hospitalized),
    died = normalise_binary(died),
    died_covid = normalise_binary(died_covid),
    age_group = case_when(
      is.na(case_age) | case_age < 0 | case_age > 110 ~ NA_character_,
      case_age <= 17 ~ "0-17",
      case_age <= 29 ~ "18-29",
      case_age <= 44 ~ "30-44",
      case_age <= 64 ~ "45-64",
      TRUE ~ "65+"
    ),
    age_group = factor(age_group, levels = age_group_levels),
    specimen_week = as.Date(floor_date(pos_sampledt, "week", week_start = 1)),
    admission_week = as.Date(floor_date(hosp_admidt, "week", week_start = 1)),
    death_week = as.Date(floor_date(died_dt, "week", week_start = 1))
  )

analysis_start <- as.Date(floor_date(
  min(covid_analysis$pos_sampledt, na.rm = TRUE), "week", week_start = 1
))
latest_specimen_date <- as.Date(max(covid_analysis$pos_sampledt, na.rm = TRUE))
latest_report_date <- as.Date(max(covid_analysis$reprt_creationdt, na.rm = TRUE))
final_week_start <- as.Date(floor_date(latest_specimen_date, "week", week_start = 1))

latest_complete_week_start <- if (
  latest_specimen_date >= as.Date(final_week_start) + 6
) final_week_start else final_week_start - weeks(1)

latest_complete_week_end <- as.Date(latest_complete_week_start) + 6
latest_complete_week_label <- paste0(
  format(as.Date(latest_complete_week_start), "%b %d"), "-",
  format(latest_complete_week_end, "%b %d, %Y")
)

week_calendar <- tibble(
  week_start = seq(analysis_start, final_week_start, by = "week")
)

weekly_cases <- covid_analysis |>
  filter(!is.na(specimen_week)) |>
  count(week_start = specimen_week, name = "cases")

# Admission events require both a Yes status and a usable event date. Dates
# outside the observed reporting window are treated as synthetic data errors.
weekly_admissions <- covid_analysis |>
  filter(
    hospitalized == "Yes",
    !is.na(admission_week),
    admission_week >= analysis_start,
    as.Date(hosp_admidt) <= latest_report_date
  ) |>
  count(week_start = admission_week, name = "admissions")

weekly_deaths <- covid_analysis |>
  filter(
    died_covid == "Yes",
    !is.na(death_week),
    death_week >= analysis_start,
    as.Date(died_dt) <= latest_report_date
  ) |>
  count(week_start = death_week, name = "deaths")

weekly_events <- week_calendar |>
  left_join(weekly_cases, by = "week_start") |>
  left_join(weekly_admissions, by = "week_start") |>
  left_join(weekly_deaths, by = "week_start") |>
  mutate(
    across(c(cases, admissions, deaths), ~ replace_na(.x, 0L)),
    week_label = format(as.Date(week_start), "%Y-%m-%d"),
    is_incomplete = week_start > latest_complete_week_start,
    cases_complete = if_else(!is_incomplete, cases, NA_integer_),
    cases_incomplete = if_else(is_incomplete, cases, NA_integer_),
    cases_wow_n = cases - lag(cases),
    cases_wow_pct = safe_pct_change(cases, lag(cases)),
    admissions_wow_n = admissions - lag(admissions),
    admissions_wow_pct = safe_pct_change(admissions, lag(admissions)),
    deaths_wow_n = deaths - lag(deaths),
    deaths_wow_pct = safe_pct_change(deaths, lag(deaths))
  )

# The explicit row lookup keeps KPI comparisons tied to the common complete week.
latest_week_index <- match(latest_complete_week_start, weekly_events$week_start)
prior_week_index <- latest_week_index - 1L
latest_complete_metrics <- weekly_events |>
  filter(week_start == latest_complete_week_start) |>
  mutate(
    cases_wow_label = format_wow(cases, weekly_events$cases[prior_week_index]),
    admissions_wow_label = format_wow(admissions, weekly_events$admissions[prior_week_index]),
    deaths_wow_label = format_wow(deaths, weekly_events$deaths[prior_week_index]),
    cases_kpi_change = format_kpi_change(cases, weekly_events$cases[prior_week_index]),
    admissions_kpi_change = format_kpi_change(admissions, weekly_events$admissions[prior_week_index]),
    deaths_kpi_change = format_kpi_change(deaths, weekly_events$deaths[prior_week_index])
  )

valid_reporting_lags <- as.numeric(difftime(
  covid_analysis$reprt_creationdt,
  covid_analysis$pos_sampledt,
  units = "days"
))
valid_reporting_lags <- valid_reporting_lags[
  !is.na(valid_reporting_lags) & valid_reporting_lags >= 0
]
reporting_lag_median <- median(valid_reporting_lags)
reporting_lag_p95 <- unname(quantile(valid_reporting_lags, 0.95))

age_severity <- covid_analysis |>
  filter(!is.na(age_group)) |>
  group_by(age_group, .drop = FALSE) |>
  summarise(
    cases = n(),
    hospitalized_n = sum(hospitalized == "Yes", na.rm = TRUE),
    hospitalized_known_n = sum(hospitalized %in% c("Yes", "No")),
    covid_deaths_n = sum(died_covid == "Yes", na.rm = TRUE),
    death_known_n = sum(died_covid %in% c("Yes", "No")),
    .groups = "drop"
  ) |>
  mutate(
    hospitalization_rate = 100 * hospitalized_n / hospitalized_known_n,
    hospitalization_complete_pct = 100 * hospitalized_known_n / cases,
    death_rate = 100 * covid_deaths_n / death_known_n,
    death_complete_pct = 100 * death_known_n / cases,
    hospitalization_detail = paste0(
      format(hospitalized_n, big.mark = ","), " / ",
      format(hospitalized_known_n, big.mark = ","), " known; ",
      sprintf("%.1f%% complete", hospitalization_complete_pct)
    ),
    death_detail = paste0(
      format(covid_deaths_n, big.mark = ","), " / ",
      format(death_known_n, big.mark = ","), " known; ",
      sprintf("%.1f%% complete", death_complete_pct)
    )
  )

overall_hospitalization_completeness <- 100 *
  sum(covid_analysis$hospitalized %in% c("Yes", "No")) / nrow(covid_analysis)
overall_death_completeness <- 100 *
  sum(covid_analysis$died_covid %in% c("Yes", "No")) / nrow(covid_analysis)

age_65_summary <- age_severity |>
  filter(age_group == "65+") |>
  mutate(
    case_share = 100 * cases / sum(age_severity$cases),
    hospitalization_share = 100 * hospitalized_n / sum(age_severity$hospitalized_n),
    death_share = 100 * covid_deaths_n / sum(age_severity$covid_deaths_n)
  )

missing_specimen_n <- sum(is.na(covid_analysis$pos_sampledt))
hospitalized_missing_specimen_n <- sum(
  covid_analysis$hospitalized == "Yes" & is.na(covid_analysis$pos_sampledt),
  na.rm = TRUE
)
excluded_admission_dates_n <- sum(
  covid_analysis$hospitalized == "Yes" &
    !is.na(covid_analysis$hosp_admidt) &
    as.Date(covid_analysis$hosp_admidt) > latest_report_date,
  na.rm = TRUE
)
