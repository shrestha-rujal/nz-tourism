library(tidyverse)
library(fastDummies)

# setwd("/home/rujal/Projects/nz-tourism/mlr")

survey <- readRDS("../decision-tree/output/survey_cleaned.rds")

accommodation <- read_csv("../decision-tree/output/accommodation_processed.csv")
activities <- read_csv("../decision-tree/output/activities_processed.csv")
decision <- read_csv("../decision-tree/output/decision_process_processed.csv")
maori_experience <- read_csv("../decision-tree/output/maori_experience_processed.csv")
mobility <- readRDS("../decision-tree/output/mobility_processed.rds")
other_countries <- read_csv("../decision-tree/output/other_countries_processed.csv")
region_visits <- read_csv("../decision-tree/output/region_visits_processed.csv")
transport <- read_csv("../decision-tree/output/transport_processed.csv")
travel_party <- read_csv("../decision-tree/output/travel_party_processed.csv")

survey_cleaned <- survey |>
  select(-sustainability_considered, -arrival_date, -arrival_month) |>
  filter(
    arrival_year != 2021
  ) |>
  mutate(
    country_of_residence = fct_lump_min(
      country_of_residence,
      min = 100,
      other_level = "Other"
    ),
    package_deal = case_when(
      package_deal == "Yes" ~ 1L,
      package_deal == "No" ~ 0L,
      TRUE ~ NA_integer_
    ),
    first_nz_trip = case_when(
      first_nz_trip == "Yes" ~ 1L,
      first_nz_trip == "No" ~ 0L,
      TRUE ~ NA_integer_
    ),
    log_treated_spend = log(treated_spend),
    log_days_in_nz = log(no_days_in_nz)
  ) |>
  dummy_cols(
    select_columns = c(
      "departure_location",
      "country_of_residence",
      "gender",
      "arrival_location",
      "age_range",
      "travel_type",
      "arrival_year",
      "arrival_season",
      "visit_purpose"
    ),
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
  ) |>
  janitor::clean_names() |>
  drop_na(package_deal)

dim(survey_cleaned) # 27,638 * 80

survey_merged <- survey_cleaned |>
  left_join(accommodation, by = "response_id") |>
  mutate(across(starts_with("accomm_"), ~ replace_na(., 0))) |>
  left_join(activities, by = "response_id") |>
  mutate(across(starts_with("activity_"), ~ replace_na(., 0))) |>
  left_join(decision, by = "response_id") |>
  mutate(across(starts_with("decision_"), ~ replace_na(., 0))) |>
  left_join(maori_experience, by = "response_id") |>
  mutate(across(starts_with("maori_exp_"), ~ replace_na(., 0))) |>
  left_join(mobility, by = "response_id") |>
  mutate(across(starts_with("mobility_"), ~ replace_na(., median(., na.rm = TRUE)))) |>
  left_join(other_countries, by = "response_id") |>
  mutate(across(starts_with("other_countries_"), ~ replace_na(., 0))) |>
  left_join(region_visits, by = "response_id") |>
  mutate(across(starts_with("itinerary_"), ~ replace_na(., 0))) |>
  left_join(transport, by = "response_id") |>
  mutate(across(starts_with("transport_"), ~ replace_na(., 0))) |>
  left_join(travel_party, by = "response_id")

dim(survey_merged) # 27,934 * 235

# saveRDS(survey_merged, "output/mlr_merged.rds")
# write_csv(survey_merged, "output/mlr_merged.csv")
