library(tidyverse)
library(Metrics)

setwd("/Users/rujalshrestha/Projects/nz-tourism/linear-regression")

# ══════════════════════════════════════════════════════════════════════════════
# PHASE 6: MODEL EVALUATION (TEST SET)
# ══════════════════════════════════════════════════════════════════════════════

# Load model and data
model <- readRDS("output/fitted_model.rds")
data_list <- readRDS("output/train_test_data.rds")
X_train <- data_list$X_train
y_train <- data_list$y_train
X_test <- data_list$X_test
y_test <- data_list$y_test

cat("═══════════════════════════════════════════════════════════════\n")
cat("MODEL EVALUATION (Test Set)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 5.1 PREDICTIONS ON TEST SET
# ──────────────────────────────────────────────────────────────────────────────

y_pred_test <- predict(model, newdata = X_test)
y_pred_train <- predict(model, newdata = X_train)

cat("Predictions generated on both train and test sets.\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 5.2 CALCULATE ERROR METRICS
# ──────────────────────────────────────────────────────────────────────────────

cat("ERROR METRICS:\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Calculate metrics for test set
mae_test <- mae(y_test, y_pred_test)
rmse_test <- rmse(y_test, y_pred_test)
mape_test <- mape(y_test, y_pred_test)

# R-squared on test set
ss_res_test <- sum((y_test - y_pred_test)^2)
ss_tot_test <- sum((y_test - mean(y_test))^2)
r2_test <- 1 - (ss_res_test / ss_tot_test)

# Adjusted R-squared on test set
n_test <- length(y_test)
k <- ncol(X_test)
adj_r2_test <- 1 - (1 - r2_test) * (n_test - 1) / (n_test - k - 1)

cat("TEST SET PERFORMANCE:\n")
cat(sprintf("  MAE (Mean Absolute Error):    NZD %.2f\n", mae_test))
cat(sprintf("  RMSE (Root Mean Squared Error): NZD %.2f\n", rmse_test))
cat(sprintf("  MAPE (Mean Absolute Pct Error): %.2f%%\n", mape_test * 100))
cat(sprintf("  R²:                            %.4f\n", r2_test))
cat(sprintf("  Adjusted R²:                   %.4f\n", adj_r2_test))

# Metrics for training set for comparison
mae_train <- mae(y_train, y_pred_train)
rmse_train <- rmse(y_train, y_pred_train)

ss_res_train <- sum((y_train - y_pred_train)^2)
ss_tot_train <- sum((y_train - mean(y_train))^2)
r2_train <- 1 - (ss_res_train / ss_tot_train)

n_train <- length(y_train)
adj_r2_train <- 1 - (1 - r2_train) * (n_train - 1) / (n_train - k - 1)

cat("\nTRAIN SET PERFORMANCE (for comparison):\n")
cat(sprintf("  MAE:                           NZD %.2f\n", mae_train))
cat(sprintf("  RMSE:                          NZD %.2f\n", rmse_train))
cat(sprintf("  R²:                            %.4f\n", r2_train))
cat(sprintf("  Adjusted R²:                   %.4f\n", adj_r2_train))

# ──────────────────────────────────────────────────────────────────────────────
# 5.3 TRAIN VS TEST COMPARISON (OVERFITTING CHECK)
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\nOVERFITTING CHECK:\n")
cat("─────────────────────────────────────────────────────────────\n")

r2_diff <- r2_train - r2_test
rmse_diff <- rmse_test - rmse_train

cat(sprintf("  R² difference (train - test): %.4f\n", r2_diff))
cat(sprintf("  RMSE difference (test - train): NZD %.2f\n", rmse_diff))

if (r2_diff > 0.05 && r2_diff < 0.15) {
  cat(sprintf("  ✓ Minimal overfitting (R² diff = %.4f)\n", r2_diff))
} else if (r2_diff < 0.05) {
  cat(sprintf("  ✓ Excellent generalization (R² diff = %.4f)\n", r2_diff))
} else if (r2_diff > 0.15) {
  cat(sprintf("  ⚠️  Possible overfitting (R² diff = %.4f)\n", r2_diff))
}

# ──────────────────────────────────────────────────────────────────────────────
# 5.4 VISUALIZATION: PREDICTED VS ACTUAL
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\nGenerating prediction plots...\n")

# Test set predictions
p1 <- tibble(actual = y_test, predicted = y_pred_test) |>
  ggplot(aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", colour = "blue", fill = "blue", alpha = 0.15) +
  labs(
    title = "Test Set: Predicted vs Actual Spending",
    x = "Actual Spending (NZD)",
    y = "Predicted Spending (NZD)",
    caption = sprintf("R² = %.4f, RMSE = NZD %.2f", r2_test, rmse_test)
  ) +
  coord_equal() +
  theme_minimal(base_size = 11)

print(p1)
ggsave("results/plots/predictions_vs_actual.png", p1, width = 8, height = 7, dpi = 300)

# Distribution of residuals on test set
test_residuals <- y_test - y_pred_test

p2 <- tibble(residuals = test_residuals) |>
  ggplot(aes(x = residuals)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red", linewidth = 1) +
  labs(
    title = "Test Set: Distribution of Prediction Residuals",
    x = "Residuals (Actual - Predicted)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 11)

print(p2)
ggsave("results/plots/test_residuals_distribution.png", p2, width = 8, height = 6, dpi = 300)

# ──────────────────────────────────────────────────────────────────────────────
# 5.5 PERFORMANCE SUMMARY TABLE
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\nPERFORMANCE SUMMARY TABLE:\n")
cat("─────────────────────────────────────────────────────────────\n\n")

perf_summary <- tibble(
  Metric = c("R²", "Adjusted R²", "RMSE (NZD)", "MAE (NZD)", "MAPE (%)"),
  Train = c(
    sprintf("%.4f", r2_train),
    sprintf("%.4f", adj_r2_train),
    sprintf("%.2f", rmse_train),
    sprintf("%.2f", mae_train),
    sprintf("%.2f", (mae_train / mean(y_train)) * 100)
  ),
  Test = c(
    sprintf("%.4f", r2_test),
    sprintf("%.4f", adj_r2_test),
    sprintf("%.2f", rmse_test),
    sprintf("%.2f", mae_test),
    sprintf("%.2f", mape_test * 100)
  )
)

print(perf_summary)

# ──────────────────────────────────────────────────────────────────────────────
# 5.6 SAVE EVALUATION RESULTS
# ──────────────────────────────────────────────────────────────────────────────

eval_results <- list(
  y_pred_test = y_pred_test,
  y_pred_train = y_pred_train,
  y_test = y_test,
  y_train = y_train,
  mae_test = mae_test,
  rmse_test = rmse_test,
  mape_test = mape_test,
  r2_test = r2_test,
  adj_r2_test = adj_r2_test,
  mae_train = mae_train,
  rmse_train = rmse_train,
  r2_train = r2_train,
  adj_r2_train = adj_r2_train,
  perf_summary = perf_summary
)

saveRDS(eval_results, "output/evaluation_results.rds")

cat("\n\n✓ Evaluation complete and results saved.\n")
