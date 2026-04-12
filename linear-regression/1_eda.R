library(tidyverse)
library(skimr)
library(corrplot)
library(moments)
library(car)

setwd("/Users/rujalshrestha/Projects/nz-tourism/linear-regression")

# ══════════════════════════════════════════════════════════════════════════════
# PHASE 1-2: DATA LOADING, INSPECTION & EDA
# ══════════════════════════════════════════════════════════════════════════════

# Load preprocessed model data from decision-tree output
df <- readRDS("../decision-tree/output/merged/model_data.rds")

cat("Data dimensions:", nrow(df), "rows x", ncol(df), "columns\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 1.1 INSPECT DATA
# ──────────────────────────────────────────────────────────────────────────────

cat("Column types:\n")
print(glimpse(df))

cat("\n\nMissing values:\n")
missing_summary <- df |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    everything(),
    names_to = "column",
    values_to = "na_count"
  ) |>
  filter(na_count > 0) |>
  arrange(desc(na_count))

print(missing_summary)

# ──────────────────────────────────────────────────────────────────────────────
# 1.2 UNIVARIATE ANALYSIS: TREATED_SPEND (OUTCOME VARIABLE)
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n=== TREATED_SPEND DISTRIBUTION ===\n")

spend_stats <- df |>
  summarise(
    min = min(treated_spend, na.rm = TRUE),
    q1 = quantile(treated_spend, 0.25, na.rm = TRUE),
    median = median(treated_spend, na.rm = TRUE),
    mean = mean(treated_spend, na.rm = TRUE),
    q3 = quantile(treated_spend, 0.75, na.rm = TRUE),
    max = max(treated_spend, na.rm = TRUE),
    sd = sd(treated_spend, na.rm = TRUE),
    skewness = skewness(treated_spend, na.rm = TRUE),
    kurtosis = kurtosis(treated_spend, na.rm = TRUE)
  )

print(spend_stats)

# Plot distribution
p_hist <- df |>
  ggplot(aes(x = treated_spend)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(
    xintercept = median(df$treated_spend, na.rm = TRUE),
    colour = "red", linetype = "dashed", linewidth = 1
  ) +
  labs(
    title = "Distribution of Treated Spend",
    x = "Treated Spend (NZD)",
    y = "Frequency",
    subtitle = "Red dashed line = median"
  ) +
  theme_minimal(base_size = 12)

# Plot density
p_density <- df |>
  ggplot(aes(x = treated_spend)) +
  geom_density(fill = "steelblue", alpha = 0.6, colour = "steelblue4") +
  geom_vline(
    xintercept = median(df$treated_spend, na.rm = TRUE),
    colour = "red", linetype = "dashed", linewidth = 1
  ) +
  labs(
    title = "Density Plot of Treated Spend",
    x = "Treated Spend (NZD)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)

# Plot log-transformed
p_log <- df |>
  filter(treated_spend > 0) |>
  ggplot(aes(x = log(treated_spend))) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(
    xintercept = median(log(df$treated_spend[df$treated_spend > 0]), na.rm = TRUE),
    colour = "red", linetype = "dashed", linewidth = 1
  ) +
  labs(
    title = "Distribution of Log(Treated Spend)",
    x = "Log(Treated Spend)",
    y = "Frequency",
    subtitle = "Red dashed line = median"
  ) +
  theme_minimal(base_size = 12)

# Boxplot to check outliers
p_box <- df |>
  ggplot(aes(y = treated_spend)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Boxplot of Treated Spend (outliers shown)",
    y = "Treated Spend (NZD)"
  ) +
  theme_minimal(base_size = 12)

print(p_hist)
print(p_density)
print(p_log)
print(p_box)

# Count outliers
outlier_threshold_low <- quantile(df$treated_spend, 0.01, na.rm = TRUE)
outlier_threshold_high <- quantile(df$treated_spend, 0.99, na.rm = TRUE)
n_outliers <- sum(df$treated_spend < outlier_threshold_low | df$treated_spend > outlier_threshold_high, na.rm = TRUE)
cat("\n\nOutliers (< 1st percentile or > 99th percentile):", n_outliers, "\n")

# ──────────────────────────────────────────────────────────────────────────────
# 1.3 FEATURE ENGINEERING: SELECT TOP CATEGORIES
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n=== FEATURE SELECTION: KEEPING TOP CATEGORIES ===\n")

# Keep top 10 countries
top_countries <- df |>
  count(country_of_residence) |>
  slice_max(n, n = 10) |>
  pull(country_of_residence)

cat("Top 10 countries:\n")
print(top_countries)

# Keep top 15 activities
activity_cols <- colnames(df)[str_starts(colnames(df), "activity_")]
activity_sums <- df |>
  select(all_of(activity_cols)) |>
  colSums(na.rm = TRUE) |>
  sort(decreasing = TRUE)

top_activities <- names(activity_sums)[1:15]
cat("\nTop 15 activities:\n")
print(top_activities)

# Keep top 10 transport methods
transport_cols <- colnames(df)[str_starts(colnames(df), "transport_")]
transport_sums <- df |>
  select(all_of(transport_cols)) |>
  colSums(na.rm = TRUE) |>
  sort(decreasing = TRUE)

top_transport <- names(transport_sums)[1:10]
cat("\nTop 10 transport methods:\n")
print(top_transport)

# Keep top 10 decision factors
decision_cols <- colnames(df)[str_starts(colnames(df), "decision_")]
decision_sums <- df |>
  select(all_of(decision_cols)) |>
  colSums(na.rm = TRUE) |>
  sort(decreasing = TRUE)

top_decision <- names(decision_sums)[1:10]
cat("\nTop 10 decision factors:\n")
print(top_decision)

# Keep top 8 accommodation types
accomm_cols <- colnames(df)[str_starts(colnames(df), "accomm_")]
accomm_sums <- df |>
  select(all_of(accomm_cols)) |>
  colSums(na.rm = TRUE) |>
  sort(decreasing = TRUE)

top_accomm <- names(accomm_sums)[1:8]
cat("\nTop 8 accommodation types:\n")
print(top_accomm)

# ──────────────────────────────────────────────────────────────────────────────
# 1.4 CREATE DERIVED FEATURES
# ──────────────────────────────────────────────────────────────────────────────

df_features <- df |>
  # Create num_regions_visited instead of 16 region dummies
  mutate(
    num_regions_visited = rowSums(
      pick(starts_with("itinerary_region_")),
      na.rm = TRUE
    ),
    # Total number of activities
    num_activities = rowSums(
      pick(starts_with("activity_")),
      na.rm = TRUE
    ),
    # Total transport methods used
    num_transport_methods = rowSums(
      pick(starts_with("transport_")),
      na.rm = TRUE
    ),
    # Country grouping: top 10 or "other"
    country_group = if_else(
      country_of_residence %in% top_countries,
      country_of_residence,
      "Other"
    ),
    # Total no. of decision factors
    num_decision_factors = rowSums(
      pick(starts_with("decision_")),
      na.rm = TRUE
    ),
    # Log-transformed spend (for later use)
    log_treated_spend = log(treated_spend)
  ) |>
  # Drop region dummies (using count instead)
  select(-starts_with("itinerary_region_")) |>
  # Keep only top activities, transport, decision, accomm dummies
  select(
    -all_of(setdiff(activity_cols, top_activities)),
    -all_of(setdiff(transport_cols, top_transport)),
    -all_of(setdiff(decision_cols, top_decision)),
    -all_of(setdiff(accomm_cols, top_accomm))
  )

cat(sprintf("\nFinal dataset: %d rows x %d columns\n", nrow(df_features), ncol(df_features)))

# ──────────────────────────────────────────────────────────────────────────────
# 1.5 BIVARIATE ANALYSIS: CONTINUOUS PREDICTORS vs OUTCOME
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n=== BIVARIATE ANALYSIS: CONTINUOUS PREDICTORS ===\n")

# Select continuous predictors
continuous_vars <- c(
  "no_days_in_nz", "age_range", "num_regions_visited", "num_activities",
  "num_transport_methods", "num_decision_factors"
)

# Scatter plots for continuous variables
for (var in continuous_vars) {
  if (var %in% colnames(df_features)) {
    p <- df_features |>
      filter(!is.na(treated_spend), !is.na(!!sym(var))) |>
      ggplot(aes(x = !!sym(var), y = treated_spend)) +
      geom_point(alpha = 0.3, size = 1) +
      geom_smooth(method = "loess", colour = "red", fill = "red", alpha = 0.2) +
      labs(
        title = paste(var, "vs Treated Spend"),
        x = var,
        y = "Treated Spend (NZD)"
      ) +
      theme_minimal(base_size = 11)
    print(p)
  }
}

# Box plots for categorical predictors
cat_vars <- c("gender", "first_nz_trip", "travel_type", "arrival_season", "country_group")

for (var in cat_vars) {
  if (var %in% colnames(df_features)) {
    p <- df_features |>
      filter(!is.na(treated_spend), !is.na(!!sym(var))) |>
      ggplot(aes(x = !!sym(var), y = treated_spend, fill = !!sym(var))) +
      geom_boxplot(alpha = 0.7) +
      coord_flip() +
      labs(
        title = paste(var, "vs Treated Spend"),
        x = var,
        y = "Treated Spend (NZD)"
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")
    print(p)
  }
}

# ──────────────────────────────────────────────────────────────────────────────
# 1.6 MULTICOLLINEARITY TESTING: CORRELATION & VIF
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n=== MULTICOLLINEARITY ANALYSIS ===\n")

# Select numeric columns for correlation
numeric_cols <- df_features |>
  select(where(is.numeric)) |>
  colnames()

# Remove response variable from this check for now
numeric_for_cor <- numeric_cols[numeric_cols != "treated_spend" & numeric_cols != "log_treated_spend"]

# Create correlation matrix
cor_matrix <- df_features |>
  select(all_of(numeric_for_cor)) |>
  drop_na() |>
  cor(use = "pairwise.complete.obs")

# Find high correlations
high_cor <- cor_matrix |>
  as.data.frame() |>
  rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "r") |>
  filter(var1 < var2, abs(r) > 0.7) |>
  arrange(desc(abs(r)))

if (nrow(high_cor) > 0) {
  cat("High correlations (|r| > 0.7):\n")
  print(high_cor)
} else {
  cat("No high correlations (|r| > 0.7) found.\n")
}

# Plot correlation matrix
png("results/plots/correlation_matrix.png", width = 1200, height = 1000, res = 120)
corrplot(cor_matrix,
  method = "color",
  type = "lower",
  addCoef.col = "black",
  number.cex = 0.7,
  tl.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  col = colorRampPalette(c("#C0392B", "white", "#2471A3"))(200),
  title = "Correlation Matrix of Numeric Predictors",
  mar = c(0, 0, 2, 0)
)
dev.off()

cat("\nCorrelation plot saved.\n")

# Save the prepared dataset for next script
saveRDS(df_features, "output/df_features.rds")

cat("\n\n✓ EDA complete. Features selected and saved.\n")
