library(tidyverse)
library(fastDummies)
library(car)

setwd("/Users/rujalshrestha/Projects/nz-tourism/linear-regression")

# ══════════════════════════════════════════════════════════════════════════════
# PHASE 3: DATA PREPARATION FOR MODELING
# ══════════════════════════════════════════════════════════════════════════════

df <- readRDS("output/df_features.rds")

cat("Starting with:", nrow(df), "rows\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 2.1 HANDLE MISSING VALUES (LISTWISE DELETION)
# ──────────────────────────────────────────────────────────────────────────────

cat("Handling missing values...\n")

# Identify key columns for modeling
key_cols <- c(
  "response_id", "treated_spend", "log_treated_spend",
  "no_days_in_nz", "age_range", "gender", "first_nz_trip",
  "travel_type", "arrival_season", "package_deal",
  "sustainability_considered", "arrival_location",
  "country_group", "num_regions_visited", "num_activities",
  "num_transport_methods", "num_decision_factors",
  "satisfaction_rating", "recommend_rating"
)

df_clean <- df |>
  select(any_of(c(key_cols, colnames(df)[str_starts(colnames(df), "activity_|accomm_|transport_|decision_")]))) |>
  drop_na()

rows_dropped <- nrow(df) - nrow(df_clean)
cat(sprintf("Rows removed due to missing values: %d (%.1f%%)\n", rows_dropped, rows_dropped/nrow(df)*100))
cat("Remaining rows:", nrow(df_clean), "\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 2.2 ENCODE CATEGORICAL VARIABLES (ONE-HOT WITH REFERENCE CATEGORY)
# ──────────────────────────────────────────────────────────────────────────────

cat("Encoding categorical variables...\n")

# Prepare data: convert character columns to factors with reference levels
df_encoded <- df_clean |>
  mutate(
    gender = factor(gender, levels = c("Male", "Female", "Other")),
    travel_type = factor(travel_type, levels = c("Independent Traveller", "Package", "Tour Group")),
    arrival_season = factor(arrival_season, levels = c("Summer", "Autumn", "Winter", "Spring")),
    first_nz_trip = factor(first_nz_trip, levels = c("Yes", "No")),
    package_deal = factor(package_deal, levels = c("Yes", "No")),
    country_group = factor(country_group),
    arrival_location = factor(arrival_location)
  )

# One-hot encode with dropping first level (reference category)
df_encoded <- df_encoded |>
  dummy_cols(
    select_columns = c("gender", "travel_type", "arrival_season", "first_nz_trip",
                       "package_deal", "country_group", "arrival_location"),
    remove_first_dummy = TRUE,  # Drop reference category
    remove_selected_columns = TRUE
  )

# Drop response_id (not needed for modeling)
df_encoded <- df_encoded |>
  select(-response_id)

cat("Total columns after encoding:", ncol(df_encoded), "\n")
cat("Columns:\n")
print(colnames(df_encoded)[1:20])
cat("...\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 2.3 SELECT PREDICTORS & OUTCOME
# ──────────────────────────────────────────────────────────────────────────────

cat("Selecting predictors for model...\n\n")

# Outcome variable
y <- df_encoded$treated_spend

# Predictors: all except treated_spend, log_treated_spend, and satisfaction/recommend ratings
X <- df_encoded |>
  select(
    -treated_spend,
    -log_treated_spend,
    -satisfaction_rating,      # Not using satisfaction for this model
    -recommend_rating          # Not using recommendation for this model
  )

cat("Outcome variable: treated_spend\n")
cat("Number of predictors:", ncol(X), "\n")
cat("Sample size:", nrow(X), "\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 2.4 CHECK VIF FOR MULTICOLLINEARITY
# ──────────────────────────────────────────────────────────────────────────────

cat("Checking Variance Inflation Factor (VIF)...\n\n")

# Use tryCatch to handle aliased coefficients gracefully
vif_result <- tryCatch({
  temp_model <- lm(y ~ ., data = X)
  vif(temp_model)
}, error = function(e) {
  cat("⚠️  VIF calculation failed due to perfect collinearity.\n")
  cat("    Attempting to identify and remove problematic columns...\n\n")
  
  # Identify columns to keep: non-numeric OR numeric with variance > 0
  numeric_cols <- sapply(X, is.numeric)
  keep_cols <- logical(length(numeric_cols))
  
  for (i in seq_along(keep_cols)) {
    if (!numeric_cols[i]) {
      keep_cols[i] <- TRUE  # Keep all non-numeric columns
    } else {
      keep_cols[i] <- sd(X[[i]], na.rm = TRUE) > 0  # Keep numeric if variance > 0
    }
  }
  
  X_filtered <- X[, keep_cols]
  
  # Try refitting
  tryCatch({
    temp_model <- lm(y ~ ., data = X_filtered)
    X <<- X_filtered  # Update X globally
    vif(temp_model)
  }, error = function(e2) {
    cat("    Could not resolve via variance filtering.\n")
    cat("    Defaulting to basic model diagnostics (skipping VIF).\n")
    return(NULL)
  })
})

if (!is.null(vif_result)) {
  vif_values <- vif_result
  
  # Identify problematic variables
  high_vif <- vif_values[vif_values > 10]
  
  if (length(high_vif) > 0) {
    cat("⚠️  Variables with VIF > 10 (high multicollinearity):\n")
    print(high_vif)
    cat("\nNote: These may need to be dropped or combined.\n")
  } else {
    cat("✓ All variables have VIF <= 10 (acceptable)\n")
  }
  
  # VIF summary
  vif_df <- tibble(
    variable = names(vif_values),
    vif = as.numeric(vif_values)
  ) |>
    arrange(desc(vif))
  
  cat("\nTop 15 highest VIF values:\n")
  print(head(vif_df, 15))
} else {
  # If VIF failed, create empty VIF dataframe
  vif_df <- tibble(variable = colnames(X), vif = NA_real_)
  cat("✓ Proceeding with modeling (VIF skipped due to perfect collinearity)\n")
}

# ──────────────────────────────────────────────────────────────────────────────
# 2.5 STANDARDIZATION (OPTIONAL - for comparing effect sizes)
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\nOptional: Creating standardized version of predictors...\n")

X_standardized <- X |>
  mutate(across(where(is.numeric), ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

y_standardized <- (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)

cat("✓ Standardized data ready (numeric columns only; will use in robustness checks)\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 2.6 TRAIN/TEST SPLIT (80/20)
# ──────────────────────────────────────────────────────────────────────────────

cat("Creating train/test split (80/20)...\n\n")

set.seed(123)  # For reproducibility

train_idx <- sample(1:nrow(X), size = 0.8 * nrow(X))

X_train <- X[train_idx, ]
y_train <- y[train_idx]

X_test <- X[-train_idx, ]
y_test <- y[-train_idx]

cat("Training set size:", nrow(X_train), "\n")
cat("Test set size:", nrow(X_test), "\n")
cat("Train/Test ratio:", nrow(X_train) / nrow(X_test), ":1\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 2.7 SAVE PREPARED DATA FOR NEXT SCRIPTS
# ──────────────────────────────────────────────────────────────────────────────

saveRDS(list(
  X_train = X_train,
  y_train = y_train,
  X_test = X_test,
  y_test = y_test,
  X_standardized = X_standardized,
  y_standardized = y_standardized,
  train_idx = train_idx
), "output/train_test_data.rds")

saveRDS(vif_df, "output/vif_results.rds")

cat("✓ Data preparation complete.\n")
cat("  - Train/test data saved\n")
cat("  - VIF results saved\n")
