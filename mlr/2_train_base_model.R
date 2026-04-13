library(tidyverse)

# setwd("/Users/rujalshrestha/Projects/nz-tourism/mlr")

model_df <- readRDS("output/mlr_merged.rds")

df <- model_df |>
  select(
    -treated_spend,
    -no_days_in_nz,
    -response_id
  )

# saveRDS(df, "output/model_df.rds")

set.seed(42)

n <- nrow(df)
train_idx <- sample.int(n, size = floor(0.80 * n), replace = FALSE)

train_df <- df[train_idx, ]
test_df <- df[-train_idx, ]

cat("Train rows: ", nrow(train_df)) # 22,110
cat("Test rows: ", nrow(test_df)) # 5,528

ols_full <- lm(log_treated_spend ~ ., data = train_df)

y_pred <- predict(ols_full, newdata = test_df)
y_actual <- test_df$log_treated_spend

rmse_log <- sqrt(mean((y_actual - y_pred)^2, na.rm = TRUE))
mae_log <- mean(abs(y_actual - y_pred), na.rm = TRUE)
r2_test <- 1 - sum((y_actual - y_pred)^2, na.rm = TRUE) / sum((y_actual - mean(y_actual, na.rm = TRUE))^2, na.rm = TRUE)

cat("\n=== Full OLS Baseline (Test Set, log scale) ===\n")
cat("RMSE:", round(rmse_log, 4), "\n")
cat("MAE :", round(mae_log, 4), "\n")
cat("R2  :", round(r2_test, 4), "\n")

# adj-r2: 0.5776
# rmse: 0.6581
full_summary <- summary(ols_full)

full_summary

coef_mat <- coef(full_summary)

coef_table <- tibble(
  term = rownames(coef_mat),
  estimate = coef_mat[, 1],
  p_value = coef_mat[, 4]
)

baseline_spend <- median(exp(train_df$log_treated_spend), na.rm = TRUE)

coef_table <- coef_table |>
  mutate(
    percent_effect = (exp(estimate) - 1) * 100,
    dollar_effect = baseline_spend * (exp(estimate) - 1)
  )

coef_table |>
  filter(term != "(Intercept)") |>
  arrange(p_value) |>
  slice_head(n = 30) |>
  arrange(desc(dollar_effect))
