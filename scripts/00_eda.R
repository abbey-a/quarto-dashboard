# Focused exploratory analysis for the synthetic COVID-19 line list
#
# Run from the repository root:
#   Rscript scripts/00_eda.R

suppressPackageStartupMessages({
  library(dplyr)
  library(janitor)
  library(lubridate)
  library(readxl)
  library(stringr)
  library(tidyr)
})

options(dplyr.summarise.inform = FALSE, width = 140)

data_path <- "data/covid_example_data.xlsx"

section <- function(title) {
  cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")
}

print_table <- function(x) {
  print(as.data.frame(x), row.names = FALSE)
}

rate_pct <- function(numerator, denominator) {
  out <- 100 * numerator / denominator
  out[!is.finite(out)] <- NA_real_
  out
}

normalise_binary <- function(x) {
  case_when(
    is.na(x) | str_trim(x) == "" ~ NA_character_,
    str_to_lower(str_trim(x)) == "yes" ~ "Yes",
    str_to_lower(str_trim(x)) == "no" ~ "No",
    TRUE ~ str_trim(x)
  )
}

age_group_levels <- c("0-17", "18-29", "30-44", "45-64", "65+", "Missing/invalid")

raw <- read_excel(data_path)
dat <- raw |>
  clean_names() |>
  rename_with(~ str_remove(.x, "_false$")) |>
  distinct() |>
  mutate(
    hospitalized = normalise_binary(hospitalized),
    died = normalise_binary(died),
    died_covid = normalise_binary(died_covid),
    age_group = case_when(
      is.na(case_age) | case_age < 0 | case_age > 110 ~ "Missing/invalid",
      case_age <= 17 ~ "0-17",
      case_age <= 29 ~ "18-29",
      case_age <= 44 ~ "30-44",
      case_age <= 64 ~ "45-64",
      TRUE ~ "65+"
    ),
    age_group = factor(age_group, levels = age_group_levels),
    specimen_week = floor_date(pos_sampledt, unit = "week", week_start = 1),
    admission_week = floor_date(hosp_admidt, unit = "week", week_start = 1),
    death_week = floor_date(died_dt, unit = "week", week_start = 1)
  )

section("1. Dataset structure")
cat("Workbook sheets:", paste(excel_sheets(data_path), collapse = ", "), "\n")
cat("Raw dimensions:", nrow(raw), "rows x", ncol(raw), "columns\n")
cat("Exact duplicate rows removed:", nrow(raw) - nrow(dat), "\n")
cat("Deduplicated dimensions:", nrow(dat), "rows x", ncol(raw), "source columns\n")
cat("Working EDA table:", nrow(dat), "rows x", ncol(dat), "columns including derived fields\n")
cat("Unique PID values:", n_distinct(dat$pid, na.rm = TRUE), "\n")
cat("Missing PID values:", sum(is.na(dat$pid) | dat$pid == ""), "\n")
cat("Duplicated nonmissing PID rows:", sum(duplicated(dat$pid) & !is.na(dat$pid)), "\n")

repeated_pid_summary <- dat |>
  add_count(pid, name = "pid_rows") |>
  filter(pid_rows > 1) |>
  group_by(pid) |>
  summarise(
    rows = n(),
    distinct_specimen_dates = n_distinct(pos_sampledt, na.rm = TRUE),
    min_specimen_date = as.Date(min(pos_sampledt, na.rm = TRUE)),
    max_specimen_date = as.Date(max(pos_sampledt, na.rm = TRUE)),
    distinct_hospitalized_values = n_distinct(hospitalized, na.rm = TRUE),
    distinct_death_values = n_distinct(died_covid, na.rm = TRUE)
  )
cat("Repeated PID groups:", nrow(repeated_pid_summary), "\n")
print_table(repeated_pid_summary)

field_inventory <- tibble(
  field = names(raw),
  cleaned_field = str_remove(make_clean_names(names(raw)), "_false$"),
  type = vapply(raw, function(x) paste(class(x), collapse = "/"), character(1))
)
print_table(field_inventory)

date_fields <- names(dat)[vapply(dat, inherits, logical(1), what = "POSIXt")]
date_ranges <- lapply(date_fields, function(v) {
  x <- dat[[v]]
  tibble(
    field = v,
    nonmissing_n = sum(!is.na(x)),
    missing_n = sum(is.na(x)),
    min_date = as.Date(if (all(is.na(x))) NA else min(x, na.rm = TRUE)),
    max_date = as.Date(if (all(is.na(x))) NA else max(x, na.rm = TRUE))
  )
}) |>
  bind_rows()
section("2. Date ranges")
print_table(date_ranges)

section("3. Missingness and basic data quality")
missingness <- tibble(
  field = names(dat)[seq_along(raw)],
  missing_n = vapply(dat[seq_along(raw)], function(x) sum(is.na(x) | (is.character(x) & str_trim(x) == "")), numeric(1)),
  missing_pct = 100 * missing_n / nrow(dat)
) |>
  arrange(desc(missing_pct), field)
print_table(missingness)

quality_checks <- tibble(
  check = c(
    "Age missing", "Age below 0", "Age above 110",
    "DOB after specimen date", "Specimen date after report creation date",
    "Symptom onset after specimen date", "Hospital admission before specimen date",
    "Hospital discharge before admission", "Death date before specimen date",
    "Hospitalized=Yes but admission date missing", "Hospitalized=No but admission date present",
    "Died=Yes but death date missing", "Died=No but death date present",
    "COVID death=Yes but died is not Yes", "Died=Yes but COVID death is missing",
    "ZIP is non-integer", "Latitude outside plausible Georgia range",
    "Longitude outside plausible Georgia range"
  ),
  n = c(
    sum(is.na(dat$case_age)), sum(dat$case_age < 0, na.rm = TRUE), sum(dat$case_age > 110, na.rm = TRUE),
    sum(dat$case_dob > dat$pos_sampledt, na.rm = TRUE),
    sum(dat$pos_sampledt > dat$reprt_creationdt, na.rm = TRUE),
    sum(dat$sym_startdt > dat$pos_sampledt, na.rm = TRUE),
    sum(dat$hosp_admidt < dat$pos_sampledt, na.rm = TRUE),
    sum(dat$hosp_dischdt < dat$hosp_admidt, na.rm = TRUE),
    sum(dat$died_dt < dat$pos_sampledt, na.rm = TRUE),
    sum(dat$hospitalized == "Yes" & is.na(dat$hosp_admidt), na.rm = TRUE),
    sum(dat$hospitalized == "No" & !is.na(dat$hosp_admidt), na.rm = TRUE),
    sum(dat$died == "Yes" & is.na(dat$died_dt), na.rm = TRUE),
    sum(dat$died == "No" & !is.na(dat$died_dt), na.rm = TRUE),
    sum(dat$died_covid == "Yes" & dat$died != "Yes", na.rm = TRUE),
    sum(dat$died == "Yes" & is.na(dat$died_covid), na.rm = TRUE),
    sum(dat$case_zip != floor(dat$case_zip), na.rm = TRUE),
    sum(!between(dat$latitude_jitt, 30.3, 35.1), na.rm = TRUE),
    sum(!between(dat$longitude_jitt, -85.7, -80.8), na.rm = TRUE)
  )
)
print_table(quality_checks)

section("4. Outcome encodings and denominator sensitivity")
for (v in c("hospitalized", "died", "died_covid", "confirmed_case", "covid_dx")) {
  cat("\n", v, "\n", sep = "")
  print_table(dat |> count(.data[[v]], name = "n", .drop = FALSE) |> mutate(pct = 100 * n / sum(n)))
}

outcome_summary <- dat |>
  summarise(
    cases = n(),
    hospitalized_yes = sum(hospitalized == "Yes", na.rm = TRUE),
    hospitalized_known = sum(hospitalized %in% c("Yes", "No")),
    hospitalized_unknown = sum(is.na(hospitalized) | !hospitalized %in% c("Yes", "No")),
    died_covid_yes = sum(died_covid == "Yes", na.rm = TRUE),
    died_covid_known = sum(died_covid %in% c("Yes", "No")),
    died_covid_unknown = sum(is.na(died_covid) | !died_covid %in% c("Yes", "No"))
  ) |>
  mutate(
    hospitalization_pct_all_cases = rate_pct(hospitalized_yes, cases),
    hospitalization_pct_known_outcome = rate_pct(hospitalized_yes, hospitalized_known),
    covid_cfr_pct_all_cases = rate_pct(died_covid_yes, cases),
    covid_cfr_pct_known_outcome = rate_pct(died_covid_yes, died_covid_known)
  )
print_table(outcome_summary)

weekly <- dat |>
  filter(!is.na(specimen_week)) |>
  group_by(specimen_week) |>
  summarise(
    cases = n(),
    hospitalized_n = sum(hospitalized == "Yes", na.rm = TRUE),
    hosp_known_n = sum(hospitalized %in% c("Yes", "No")),
    covid_deaths_n = sum(died_covid == "Yes", na.rm = TRUE),
    death_known_n = sum(died_covid %in% c("Yes", "No"))
  ) |>
  complete(specimen_week = seq(min(specimen_week), max(specimen_week), by = "week"),
           fill = list(cases = 0L, hospitalized_n = 0L, hosp_known_n = 0L,
                       covid_deaths_n = 0L, death_known_n = 0L)) |>
  arrange(specimen_week) |>
  mutate(
    hosp_pct_all_cases = rate_pct(hospitalized_n, cases),
    hosp_pct_known = rate_pct(hospitalized_n, hosp_known_n),
    cfr_pct_all_cases = rate_pct(covid_deaths_n, cases),
    cfr_pct_known = rate_pct(covid_deaths_n, death_known_n),
    cases_wow_n = cases - lag(cases),
    cases_wow_pct = rate_pct(cases - lag(cases), lag(cases)),
    hospitalized_wow_n = hospitalized_n - lag(hospitalized_n),
    hospitalized_wow_pct = rate_pct(hospitalized_n - lag(hospitalized_n), lag(hospitalized_n)),
    deaths_wow_n = covid_deaths_n - lag(covid_deaths_n),
    deaths_wow_pct = rate_pct(covid_deaths_n - lag(covid_deaths_n), lag(covid_deaths_n))
  )

section("5. Weekly specimen-date cohort trends")
min_specimen <- min(as.Date(dat$pos_sampledt), na.rm = TRUE)
max_specimen <- max(as.Date(dat$pos_sampledt), na.rm = TRUE)
last_week_start <- max(weekly$specimen_week)
last_week_end <- last_week_start + days(6)
cat("Specimen date range:", format(min_specimen), "to", format(max_specimen), "\n")
cat("Final observed reporting week:", format(last_week_start), "to", format(last_week_end), "\n")
cat("Final week reaches its Sunday endpoint:", max_specimen >= last_week_end, "\n")
cat("Latest report-creation date:", format(max(as.Date(dat$reprt_creationdt), na.rm = TRUE)), "\n")
cat("Cases with missing specimen date:", sum(is.na(dat$pos_sampledt)), "\n")
cat("Hospitalizations among missing-specimen-date cases:",
    sum(dat$hospitalized == "Yes" & is.na(dat$pos_sampledt), na.rm = TRUE), "\n")
cat("COVID deaths among missing-specimen-date cases:",
    sum(dat$died_covid == "Yes" & is.na(dat$pos_sampledt), na.rm = TRUE), "\n")

latest_complete_week <- weekly |>
  filter(as.Date(specimen_week) + 6 <= max_specimen) |>
  slice_tail(n = 1)
cat("\nLatest complete specimen week and week-over-week changes\n")
print_table(latest_complete_week |>
              select(specimen_week, cases, cases_wow_n, cases_wow_pct,
                     hospitalized_n, hospitalized_wow_n, hospitalized_wow_pct,
                     covid_deaths_n, deaths_wow_n, deaths_wow_pct))

cat("\nPeak weeks\n")
peak_summary <- bind_rows(
  weekly |> slice_max(cases, n = 1, with_ties = FALSE) |> transmute(metric = "Cases by specimen week", week = specimen_week, n = cases),
  weekly |> slice_max(hospitalized_n, n = 1, with_ties = FALSE) |> transmute(metric = "Hospitalized cases by specimen week", week = specimen_week, n = hospitalized_n),
  weekly |> slice_max(covid_deaths_n, n = 1, with_ties = FALSE) |> transmute(metric = "COVID deaths by specimen week", week = specimen_week, n = covid_deaths_n)
)
print_table(peak_summary)

cat("\nLatest 10 specimen weeks and week-over-week changes\n")
print_table(weekly |> tail(10) |> select(specimen_week, cases, cases_wow_n, cases_wow_pct,
                                         hospitalized_n, hospitalized_wow_n, hospitalized_wow_pct,
                                         covid_deaths_n, deaths_wow_n, deaths_wow_pct,
                                         hosp_pct_all_cases, hosp_pct_known,
                                         cfr_pct_all_cases, cfr_pct_known))

cat("\nLargest absolute week-over-week count changes (excluding first week)\n")
largest_changes <- bind_rows(
  weekly |> filter(!is.na(cases_wow_n)) |> slice_max(abs(cases_wow_n), n = 3) |>
    transmute(metric = "Cases", specimen_week, value = cases, wow_n = cases_wow_n, wow_pct = cases_wow_pct),
  weekly |> filter(!is.na(hospitalized_wow_n)) |> slice_max(abs(hospitalized_wow_n), n = 3) |>
    transmute(metric = "Hospitalized", specimen_week, value = hospitalized_n, wow_n = hospitalized_wow_n, wow_pct = hospitalized_wow_pct),
  weekly |> filter(!is.na(deaths_wow_n)) |> slice_max(abs(deaths_wow_n), n = 3) |>
    transmute(metric = "COVID deaths", specimen_week, value = covid_deaths_n, wow_n = deaths_wow_n, wow_pct = deaths_wow_pct)
)
print_table(largest_changes)

section("6. Age-specific severity and mortality")
age_outcomes <- dat |>
  group_by(age_group, .drop = FALSE) |>
  summarise(
    cases = n(),
    hospitalized_n = sum(hospitalized == "Yes", na.rm = TRUE),
    hosp_known_n = sum(hospitalized %in% c("Yes", "No")),
    covid_deaths_n = sum(died_covid == "Yes", na.rm = TRUE),
    death_known_n = sum(died_covid %in% c("Yes", "No")),
    median_age = median(case_age, na.rm = TRUE)
  ) |>
  mutate(
    case_share_pct = rate_pct(cases, sum(cases)),
    hospitalization_pct_all_cases = rate_pct(hospitalized_n, cases),
    hospitalization_pct_known = rate_pct(hospitalized_n, hosp_known_n),
    cfr_pct_all_cases = rate_pct(covid_deaths_n, cases),
    cfr_pct_known = rate_pct(covid_deaths_n, death_known_n)
  )
print_table(age_outcomes)

weekly_age_mix <- dat |>
  filter(!is.na(specimen_week), age_group != "Missing/invalid") |>
  count(specimen_week, age_group, name = "cases") |>
  group_by(specimen_week) |>
  mutate(week_cases = sum(cases), share_pct = 100 * cases / week_cases) |>
  ungroup()

cat("\nAge mix during the five highest-case weeks\n")
top_case_weeks <- weekly |> slice_max(cases, n = 5, with_ties = FALSE) |> pull(specimen_week)
print_table(weekly_age_mix |> filter(specimen_week %in% top_case_weeks) |>
              arrange(specimen_week, age_group))

section("7. Event-date trends, lags, and reporting intervals")
admissions_by_week <- dat |>
  filter(hospitalized == "Yes", !is.na(admission_week)) |>
  count(admission_week, name = "admissions")
deaths_by_week <- dat |>
  filter(died_covid == "Yes", !is.na(death_week)) |>
  count(death_week, name = "covid_deaths_by_death_date")

event_weekly <- weekly |>
  select(week = specimen_week, cases) |>
  full_join(admissions_by_week, by = c("week" = "admission_week")) |>
  full_join(deaths_by_week, by = c("week" = "death_week")) |>
  arrange(week) |>
  mutate(
    across(c(cases, admissions, covid_deaths_by_death_date), ~ replace_na(.x, 0L)),
    admissions_wow_n = admissions - lag(admissions),
    admissions_wow_pct = rate_pct(admissions - lag(admissions), lag(admissions)),
    death_events_wow_n = covid_deaths_by_death_date - lag(covid_deaths_by_death_date),
    death_events_wow_pct = rate_pct(covid_deaths_by_death_date - lag(covid_deaths_by_death_date),
                                    lag(covid_deaths_by_death_date))
  )

event_peaks <- bind_rows(
  event_weekly |> slice_max(cases, n = 1, with_ties = FALSE) |> transmute(metric = "Cases by specimen date", week, n = cases),
  event_weekly |> slice_max(admissions, n = 1, with_ties = FALSE) |> transmute(metric = "Admissions by admission date", week, n = admissions),
  event_weekly |> slice_max(covid_deaths_by_death_date, n = 1, with_ties = FALSE) |>
    transmute(metric = "COVID deaths by death date", week, n = covid_deaths_by_death_date)
)
print_table(event_peaks)

cat("\nLatest 10 event weeks and week-over-week changes\n")
print_table(event_weekly |>
              filter(as.Date(week) <= max_specimen) |>
              tail(10) |>
              select(week, admissions, admissions_wow_n, admissions_wow_pct,
                     covid_deaths_by_death_date, death_events_wow_n, death_events_wow_pct))

lag_correlations <- lapply(0:6, function(lag_weeks) {
  tibble(
    lag_weeks = lag_weeks,
    cases_vs_admissions = cor(event_weekly$cases,
                              dplyr::lead(event_weekly$admissions, lag_weeks),
                              use = "complete.obs"),
    cases_vs_deaths = cor(event_weekly$cases,
                          dplyr::lead(event_weekly$covid_deaths_by_death_date, lag_weeks),
                          use = "complete.obs")
  )
}) |>
  bind_rows()
cat("\nCorrelation of cases with later admission/death event counts\n")
print_table(lag_correlations)

intervals <- dat |>
  transmute(
    report_minus_specimen_days = as.numeric(difftime(reprt_creationdt, pos_sampledt, units = "days")),
    admission_minus_specimen_days = as.numeric(difftime(hosp_admidt, pos_sampledt, units = "days")),
    death_minus_specimen_days = as.numeric(difftime(died_dt, pos_sampledt, units = "days")),
    length_of_stay_days = as.numeric(difftime(hosp_dischdt, hosp_admidt, units = "days"))
  )
interval_summary <- lapply(names(intervals), function(v) {
  x <- intervals[[v]]
  tibble(
    interval = v,
    n = sum(!is.na(x)),
    negative_n = sum(x < 0, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    p25 = quantile(x, 0.25, na.rm = TRUE),
    p75 = quantile(x, 0.75, na.rm = TRUE),
    p95 = quantile(x, 0.95, na.rm = TRUE)
  )
}) |>
  bind_rows()
cat("\nTiming intervals in days\n")
print_table(interval_summary)

section("8. Geography")
geo_summary <- tibble(
  metric = c(
    "Missing ZIP", "Distinct nonmissing ZIPs", "Missing latitude", "Missing longitude",
    "Distinct coordinate pairs", "ZIPs represented by more than 100 cases"
  ),
  value = c(
    sum(is.na(dat$case_zip)), n_distinct(dat$case_zip, na.rm = TRUE),
    sum(is.na(dat$latitude_jitt)), sum(is.na(dat$longitude_jitt)),
    n_distinct(paste(dat$latitude_jitt, dat$longitude_jitt), na.rm = TRUE),
    dat |> filter(!is.na(case_zip)) |> count(case_zip) |> summarise(sum(n > 100)) |> pull()
  )
)
print_table(geo_summary)

zip_summary <- dat |>
  filter(!is.na(case_zip)) |>
  group_by(case_zip) |>
  summarise(
    cases = n(),
    hospitalized_n = sum(hospitalized == "Yes", na.rm = TRUE),
    hosp_known_n = sum(hospitalized %in% c("Yes", "No")),
    covid_deaths_n = sum(died_covid == "Yes", na.rm = TRUE),
    death_known_n = sum(died_covid %in% c("Yes", "No")),
    median_latitude = median(latitude_jitt, na.rm = TRUE),
    median_longitude = median(longitude_jitt, na.rm = TRUE)
  ) |>
  mutate(
    case_share_pct = rate_pct(cases, sum(cases)),
    hospitalization_pct_known = rate_pct(hospitalized_n, hosp_known_n),
    cfr_pct_known = rate_pct(covid_deaths_n, death_known_n)
  ) |>
  arrange(desc(cases))

cat("\nTop 15 ZIPs by case count\n")
print_table(zip_summary |> slice_head(n = 15))
cat("Top 5 ZIP share of cases:", round(sum(head(zip_summary$cases, 5)) / sum(zip_summary$cases) * 100, 1), "%\n")
cat("Top 10 ZIP share of cases:", round(sum(head(zip_summary$cases, 10)) / sum(zip_summary$cases) * 100, 1), "%\n")
cat("ZIP case-count coefficient of variation:", round(sd(zip_summary$cases) / mean(zip_summary$cases), 2), "\n")
cat("Available named geographic fields: case_zip only (no city, county, tract, or state field).\n")
cat("Coordinates are explicitly jittered and should not be interpreted as exact addresses.\n")

section("9. Dashboard calculation audit")
audit <- tibble(
  current_element = c(
    "Total Cases", "Total Deaths", "New Cases (Latest Week)", "Total Hospitalized",
    "New Admissions (Latest Week)", "Weekly Hospitalization Rate",
    "Deaths and Case Fatality Rate by Week", "CDC week labels", "Race pie", "Age/sex counts"
  ),
  assessment = c(
    "Counts every deduplicated row. There are repeated PIDs, so row count is not exactly a unique-person count; confirmed_case also includes No, Pending, and missing values even though covid_dx is Confirmed for all rows.",
    "Counts died_covid=Yes across all rows; valid as a file total, but death outcome missingness should be disclosed.",
    "Uses specimen week and does not test whether the last week is complete; can falsely imply a decline.",
    "Sums only records with a nonmissing specimen date, so it is not necessarily the true file total.",
    "Not admissions: it counts hospitalized cases grouped by specimen week, regardless of admission date.",
    "Uses all cases as denominator, treating unknown hospitalization status like No; known outcomes are the cleaner denominator.",
    "Assigns deaths to specimen week, not death week. This is a cohort CFR, not a death-occurrence trend; unknown outcomes and right censoring can bias recent rates.",
    "Data are grouped into Monday-start weeks, but lubridate::epiweek and the JavaScript tooltip are not a consistent CDC/MMWR week implementation.",
    "A pie of cumulative counts is hard to compare and does not expose missingness or time/risk differences.",
    "Useful context, but cumulative counts mainly mirror the case age distribution and do not show severity risk."
  )
)
print_table(audit)

section("10. Reproducible takeaways")
cat("Use the printed tables above to select presentation findings. Key interpretation rules:\n")
cat("- Label specimen-week measures as case cohorts; label admission/death-date measures as events.\n")
cat("- For outcome percentages, show the known-outcome denominator and its completeness.\n")
cat("- Do not interpret the final week as a decline unless its seven-day window is complete.\n")
cat("- ZIP counts are not incidence rates without population denominators; jittered points are not exact locations.\n")
