library(tidyverse)

setwd("/home/rujal/Projects/nz-tourism/decision-tree")

nz_visitors <- readRDS("output/merged/nz_visitors.rds")
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

###############################################

# filter out rows with missing values from left-join

get_data_columns <- function(df) {
  colnames(df) |> setdiff("response_id")
}

tables_to_check <- list(
  accommodation,
  activities,
  decision,
  maori_experience,
  region_visits,
  satisfaction,
  ease,
  maori_sentiment
)

model_data <- nz_visitors

for (table in tables_to_check) {
  cols <- get_data_columns(table)
  model_data <- model_data |>
    filter(!if_all(all_of(cols), is.na))
}

dim(nz_visitors) # 27934
dim(model_data) # 19239

# write_csv(model_data, "output/merged/model_data.csv")
# saveRDS(model_data, "output/merged/model_data.rds")


###############################################

# make data ready to use in model (only columns that can be used in model)

tree_model_data <- model_data |>
  select(
    -arrival_date,
    -arrival_month,
    -expectation_rating
  )

# write_csv(tree_model_data, "output/merged/tree_model_data.csv")
# saveRDS(tree_model_data, "output/merged/tree_model_data.rds")


###############################################


# only columns specifically relevant for recommendation_tree_model

reco_tree_model_data <- tree_model_data |>
  select(
    -treated_spend,
    -satisfaction_rating
  ) |>
  filter(!is.na(recommend_rating))

dim(reco_tree_model_data)
# 19239 * 201

# write_csv(reco_tree_model_data, "output/merged/reco_tree_model_data.csv")
# saveRDS(reco_tree_model_data, "output/merged/reco_tree_model_data.rds")


# only columns specifically relevant for satisfaction_tree_model

satisfaction_tree_model_data <- tree_model_data |>
  select(
    -treated_spend,
    -recommend_rating
  ) |>
  filter(!is.na(satisfaction_rating))

dim(satisfaction_tree_model_data)
# 19239 * 201

# write_csv(satisfaction_tree_model_data, "output/merged/satisfaction_tree_model_data.csv")
# saveRDS(satisfaction_tree_model_data, "output/merged/satisfaction_tree_model_data.rds")
