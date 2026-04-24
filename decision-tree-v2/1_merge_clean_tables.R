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
expenditure <- read_csv("../decision-tree/output/expenditure_cleaned.csv")
satisfaction <- read_csv("../decision-tree/output/satisfaction_cleaned.csv")

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
    gender = case_when(
      gender %in% c("Another Gender", "Rather not say") ~ "other_or_unspecified",
      TRUE ~ gender
    ),
    across(c(
      "departure_location",
      "country_of_residence",
      "gender",
      "arrival_location",
      "age_range",
      "travel_type",
      "arrival_year",
      "arrival_season",
      "visit_purpose",
      "package_deal",
      "first_nz_trip"
    ), ~ fct_na_value_to_level(as.factor(.x), level = "Unknown"))
  ) |>
  janitor::clean_names()


dim(survey_cleaned) # 27,933 * 14

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
  left_join(travel_party, by = "response_id") |>
  left_join(expenditure, by = "response_id") |>
  left_join(satisfaction, by = "response_id")

dim(survey_merged) # 27,933 * 183

spend_cap <- quantile(survey_merged$treated_spend, 0.99, na.rm = TRUE)
print(spend_cap) # 25,126.9

cost_cols <- names(survey_merged) |> str_subset("^cost")

merged_cleaned <- survey_merged |>
  mutate(
    treated_spend_capped = pmin(treated_spend, spend_cap),
    across(
      all_of(cost_cols),
      ~ .x / treated_spend,
      .names = "share_{.col}"
    )
  ) |>
  select(
    -treated_spend,
    -expectation_rating,
    -satisfaction_rating,
    -all_of(cost_cols)
  ) |>
  filter(!is.na(recommend_rating))

dim(merged_cleaned) # 21,551 * 181

stat <- merged_cleaned |>
  select(starts_with("share_")) |>
  summarize(across(everything(), ~ round(mean(.) * 100, 1))) |>
  pivot_longer(
    cols = everything(),
    names_to = "industry",
    values_to = "pct_spend"
  ) |>
  arrange(desc(pct_spend))

# share_cost_accomm            23.8
# share_cost_eating_out        17.7
# share_cost_car_rentals       11.2
# share_cost_food_drink        10
# share_cost_shopping          9.5
# share_cost_entertainment     8.6
# share_cost_dom_travel        6.3
# share_cost_tour_package      4.4
# share_cost_dom_flights       4
# share_cost_other             3.3
# share_cost_day_cruise        1.4

# saveRDS(merged_cleaned, "output/dt2_modal.rds")
# write_csv(merged_cleaned, "output/dt2_modal.csv")
