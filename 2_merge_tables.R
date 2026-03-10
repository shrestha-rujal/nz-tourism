library(tidyverse)

survey_main <- readRDS("output/survey_cleaned.rds")
satisfaction <- read_csv("output/satisfaction_cleaned.csv")
accommodation <- read_csv("output/accommodation_processed.csv")
activities <- read_csv("output/activities_processed.csv")
decision <- read_csv("output/decision_process_processed.csv")
ease <- readRDS("output/ease_cleaned.rds")
environment <- readRDS("output/environment_processed.rds")
expenditure <- read_csv("output/expenditure_cleaned.csv")
maori_experience <- read_csv("output/maori_experience_processed.csv")
maori_sentiment <- readRDS("output/maori_sentiment_processed.rds")
mobility <- readRDS("output/mobility_processed.rds")
other_countries <- read_csv("output/other_countries_processed.csv")
poor_experiences <- readRDS("output/poor_experiences_processed.rds")
region_visits <- read_csv("output/region_visits_processed.csv")
transport <- read_csv("output/transport_processed.csv")
travel_party <- read_csv("output/travel_party_processed.csv")

colnames(survey_main)

# "response_id"
# "departure_location"
# "country_of_residence"
# "gender"
# "first_nz_trip"
# "arrival_location"
# "arrival_date"
# "no_days_in_nz"
# "package_deal"
# "age_range"
# "travel_type"
# "sustainability_considered"
# "treated_spend"
# "visit_purpose"
# "arrival_month"
# "arrival_year"
# "arrival_season"

nz_visitors <- survey_main |>
  left_join(satisfaction, by = "response_id") |>
  left_join(accommodation, by = "response_id") |>
  left_join(activities, by = "response_id") |>
  left_join(decision, by = "response_id") |>
  left_join(ease, by = "response_id") |>
  left_join(environment, by = "response_id") |>
  left_join(expenditure, by = "response_id") |>
  left_join(maori_experience, by = "response_id") |>
  left_join(maori_sentiment, by = "response_id") |>
  left_join(mobility, by = "response_id") |>
  left_join(other_countries, by = "response_id") |>
  mutate(across(c(
    visited_other_countries, no_countries_visited,
    visited_before, visited_after
  ), ~ replace_na(., 0))) |>
  left_join(poor_experiences, by = "response_id") |>
  left_join(region_visits, by = "response_id") |>
  left_join(transport, by = "response_id") |>
  mutate(across(
    c(app_based_services_such_as_uber_ola_etc:drove_themselves),
    ~ replace_na(., 0)
  )) |>
  left_join(travel_party, by = "response_id")

dim(nz_visitors)

# write_csv(nz_visitors, "output/merged/nz_visitors.csv")
# saveRDS(nz_visitors, "output/merged/nz_visitors.rds")
