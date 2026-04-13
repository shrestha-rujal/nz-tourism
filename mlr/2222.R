library(tidyverse)
library(car)  # for VIF
library(fastDummies)

setwd("/home/rujal/Projects/nz-tourism/mlr")

# Load the merged data
survey_cleaned <- readRDS("../mlr/output/mlr_merged.rds")

# ================================================================================
# STEP 1: ONE-HOT ENCODE REMAINING CATEGORICAL VARIABLES
# ================================================================================

# Agreed survey-column handling:
# - Keep age_range as categorical (dummy encode)
# - Relump country_of_residence at min = 200, then dummy encode
# - Dummy encode: departure_location, gender, first_nz_trip, arrival_location,
#   age_range, travel_type, visit_purpose, arrival_season
# - Drop arrival_date and arrival_month
# - Keep arrival_year numeric

survey_encoded <- survey_cleaned |>
  mutate(
    package_deal = case_when(
      package_deal == "Yes" ~ 1L,
      package_deal == "No" ~ 0L,
      TRUE ~ NA_integer_
    ),
    country_of_residence = forcats::fct_lump_min(
      country_of_residence,
      min = 200,
      other_level = "Other"
    )
  ) |>
  select(-arrival_date, -arrival_month) |>
  # One-hot encode categorical columns (creates n-1 dummies per variable)
  dummy_cols(
    select_columns = c(
      "departure_location",
      "country_of_residence",
      "gender",
      "first_nz_trip",
      "arrival_location",
      "age_range",
      "travel_type",
      "arrival_season",
      "visit_purpose"
    ),
    remove_first_dummy = TRUE,  # Remove first category (reference encoding)
    remove_selected_columns = TRUE
  ) |>
  # Convert to standard column names (replace spaces/special chars)
  janitor::clean_names()

# Check: should now have mostly numeric columns
dim(survey_encoded)
glimpse(survey_encoded, max_level = 15)


# ================================================================================
# STEP 2: CALCULATE CORRELATIONS WITH OUTCOME (treated_spend)
# ================================================================================

correlations <- survey_encoded |>
  select(-response_id) |>
  # Convert to numeric (in case any factors slipped through)
  mutate(across(everything(), as.numeric)) |>
  pivot_longer(-treated_spend, names_to = "feature", values_to = "value") |>
  group_by(feature) |>
  summarise(
    correlation = cor(value, treated_spend, use = "complete.obs"),
    abs_correlation = abs(correlation),
    .groups = "drop"
  ) |>
  arrange(desc(abs_correlation))

# View top 40 features by correlation strength
print(correlations, n = 40)

# ================================================================================
# STEP 3: VISUALIZE CORRELATIONS
# ================================================================================

p_corr <- correlations |>
  slice_head(n = 40) |>  # Top 40 features
  mutate(feature = fct_reorder(feature, abs_correlation)) |>
  ggplot(aes(x = abs_correlation, y = feature, fill = correlation > 0)) +
  geom_col() +
  scale_fill_manual(values = c("#E74C3C", "#3498DB"), name = "Direction") +
  labs(
    title = "Top 40 Features by Correlation with treated_spend",
    x = "Absolute Correlation",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 9)
  )

ggsave("output/top_correlations.png", p_corr, width = 10, height = 8, dpi = 300)
print(p_corr)


# ================================================================================
# STEP 4: SELECT FEATURES STRATEGICALLY
# ================================================================================

# Option 1: Top N features by correlation (e.g., top 50)
top_n_features <- correlations |>
  slice_head(n = 50) |>
  pull(feature)

# Option 2: Features with |correlation| > threshold (e.g., > 0.05)
threshold_features <- correlations |>
  filter(abs_correlation > 0.05) |>
  pull(feature)

cat("Top 50 features:", length(top_n_features), "\n")
cat("Features with |r| > 0.05:", length(threshold_features), "\n")

# Use threshold approach (more data-driven)
selected_features <- threshold_features

# Build reduced dataset
survey_reduced <- survey_encoded |>
  select(response_id, treated_spend, all_of(selected_features))

dim(survey_reduced)


# ================================================================================
# STEP 5: CHECK MULTICOLLINEARITY (VIF)
# ================================================================================

# For VIF, we need numeric predictors only (exclude response_id and outcome)
# Note: VIF > 10 suggests problematic multicollinearity; VIF > 5 is elevated

# Fit a simple OLS with all selected features
model_full <- lm(treated_spend ~ . - response_id, data = survey_reduced)

# Calculate VIF for each predictor
vif_values <- vif(model_full) |>
  as_tibble(rownames = "feature") |>
  rename(vif = value) |>
  arrange(desc(vif)) |>
  mutate(concern = case_when(
    vif > 10 ~ "High",
    vif > 5 ~ "Moderate",
    TRUE ~ "Low"
  ))

print(vif_values, n = 40)

# Identify problematic features
high_vif_features <- vif_values |>
  filter(vif > 10) |>
  pull(feature)

moderate_vif_features <- vif_values |>
  filter(vif > 5 & vif <= 10) |>
  pull(feature)

cat("\nHigh multicollinearity (VIF > 10):", length(high_vif_features), "\n")
cat("Moderate multicollinearity (5 < VIF <= 10):", length(moderate_vif_features), "\n")


# ================================================================================
# STEP 6: VISUALIZE VIF
# ================================================================================

p_vif <- vif_values |>
  slice_head(n = 40) |>
  mutate(feature = fct_reorder(feature, vif)) |>
  ggplot(aes(x = vif, y = feature, fill = concern)) +
  geom_col() +
  geom_vline(xintercept = 5, linetype = "dashed", color = "#E74C3C", size = 1) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "#C0392B", size = 1) +
  scale_fill_manual(
    values = c("Low" = "#2ECC71", "Moderate" = "#F39C12", "High" = "#E74C3C"),
    name = "Multicollinearity"
  ) +
  labs(
    title = "Variance Inflation Factor (VIF) - Top 40 Features",
    subtitle = "Dashed lines at VIF = 5 (moderate) and VIF = 10 (high)",
    x = "VIF",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "#7F8C8D"),
    axis.text.y = element_text(size = 9)
  )

ggsave("output/vif_analysis.png", p_vif, width = 10, height = 8, dpi = 300)
print(p_vif)


# ================================================================================
# STEP 7: SUMMARY & RECOMMENDATIONS
# ================================================================================

cat("\n===== FEATURE SELECTION SUMMARY =====\n")
cat("Original columns:", ncol(survey_encoded) - 1, "\n")
cat("Selected features (|r| > 0.05):", ncol(survey_reduced) - 2, "\n")
cat("Dimension reduction:", round((1 - (ncol(survey_reduced) - 2) / (ncol(survey_encoded) - 1)) * 100, 1), "%\n\n")

cat("Multicollinearity Summary:\n")
cat("  - High VIF (>10):", length(high_vif_features), "features\n")
cat("  - Moderate VIF (5-10):", length(moderate_vif_features), "features\n")
cat("  - Low VIF (<5):", nrow(vif_values) - length(high_vif_features) - length(moderate_vif_features), "features\n\n")

# Save for next step
saveRDS(survey_reduced, "output/mlr_selected_features.rds")
write_csv(survey_reduced, "output/mlr_selected_features.csv")

cat("✓ Feature selection complete!\n")
cat("  - Saved to: output/mlr_selected_features.rds\n")
cat("  - Visualizations: output/top_correlations.png, output/vif_analysis.png\n")
