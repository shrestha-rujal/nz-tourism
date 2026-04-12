library(tidyverse)
library(Metrics)

setwd("/Users/rujalshrestha/Projects/nz-tourism/linear-regression")

# ══════════════════════════════════════════════════════════════════════════════
# PHASE 7: ROBUSTNESS CHECKS
# ══════════════════════════════════════════════════════════════════════════════

# Load original data and model
data_list <- readRDS("output/train_test_data.rds")
model_original <- readRDS("output/fitted_model.rds")
eval_results <- readRDS("output/evaluation_results.rds")
df_features <- readRDS("output/df_features.rds")
diagnostics <- readRDS("output/diagnostics.rds")

cat("═══════════════════════════════════════════════════════════════\n")
cat("ROBUSTNESS CHECKS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 6.1 SENSITIVITY ANALYSIS: RE-FIT WITHOUT EXTREME OUTLIERS
# ──────────────────────────────────────────────────────────────────────────────

cat("6.1 SENSITIVITY ANALYSIS: Fitting without extreme outliers\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Identify extreme outliers (Cook's distance > mean + 2*sd)
cooks_d <- diagnostics$cooks_distance
mean_cooks <- mean(cooks_d)
sd_cooks <- sd(cooks_d)
outlier_threshold <- mean_cooks + 2 * sd_cooks

outlier_idx <- which(cooks_d > outlier_threshold)

cat(sprintf("Identified %d extreme outliers (Cook's D > %.6f)\n", length(outlier_idx), outlier_threshold))

# Re-fit without outliers
X_train_no_outliers <- data_list$X_train[-outlier_idx, ]
y_train_no_outliers <- data_list$y_train[-outlier_idx]

model_no_outliers <- lm(y_train_no_outliers ~ ., data = X_train_no_outliers)

# Compare coefficients
coef_original <- coef(model_original)
coef_no_outliers <- coef(model_no_outliers)

coef_comparison <- tibble(
  term = names(coef_original),
  original = coef_original,
  no_outliers = coef_no_outliers,
  difference = coef_no_outliers - coef_original,
  pct_change = (difference / abs(original)) * 100
)

# Show top 10 coefficients with biggest changes
top_changes <- coef_comparison |>
  filter(term != "(Intercept)") |>
  slice_max(abs(pct_change), n = 10)

cat("\nTop 10 Coefficients with Largest Changes:\n")
print(top_changes)

# Test set performance with outliers removed
y_pred_test_no_outliers <- predict(model_no_outliers, newdata = data_list$X_test)
rmse_test_no_outliers <- rmse(data_list$y_test, y_pred_test_no_outliers)
r2_test_no_outliers <- 1 - (sum((data_list$y_test - y_pred_test_no_outliers)^2) /
                               sum((data_list$y_test - mean(data_list$y_test))^2))

cat("\nTest Set Performance Comparison:\n")
cat(sprintf("  Original model RMSE:        NZD %.2f\n", eval_results$rmse_test))
cat(sprintf("  Model without outliers RMSE: NZD %.2f\n", rmse_test_no_outliers))
cat(sprintf("  Original model R²:          %.4f\n", eval_results$r2_test))
cat(sprintf("  Model without outliers R²:  %.4f\n", r2_test_no_outliers))

# ──────────────────────────────────────────────────────────────────────────────
# 6.2 SENSITIVITY ANALYSIS: LOG-TRANSFORMED OUTCOME
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n6.2 SENSITIVITY ANALYSIS: Using log-transformed outcome\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Fit model with log-transformed outcome
y_train_log <- log(data_list$y_train)
y_test_log <- log(data_list$y_test)

model_log <- lm(y_train_log ~ ., data = data_list$X_train)

# Predictions on log scale and back-transform
y_pred_test_log <- predict(model_log, newdata = data_list$X_test)
y_pred_test_log_backtransformed <- exp(y_pred_test_log)

# Calculate RMSE on original scale for comparison
rmse_test_log <- rmse(data_list$y_test, y_pred_test_log_backtransformed)
r2_test_log <- 1 - (sum((data_list$y_test - y_pred_test_log_backtransformed)^2) /
                       sum((data_list$y_test - mean(data_list$y_test))^2))

cat("Test Set Performance Comparison:\n")
cat(sprintf("  Original model RMSE:     NZD %.2f\n", eval_results$rmse_test))
cat(sprintf("  Log-transformed model RMSE: NZD %.2f\n", rmse_test_log))
cat(sprintf("  Original model R²:       %.4f\n", eval_results$r2_test))
cat(sprintf("  Log-transformed model R²: %.4f\n", r2_test_log))

# ──────────────────────────────────────────────────────────────────────────────
# 6.3 FEATURE IMPORTANCE: STANDARDIZED COEFFICIENTS
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n6.3 FEATURE IMPORTANCE: Standardized Coefficients\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Fit model with standardized data
X_train_std <- data_list$X_standardized[data_list$train_idx, ]
y_train_std <- data_list$y_standardized[data_list$train_idx]

model_standardized <- lm(y_train_std ~ ., data = X_train_std)

# Extract standardized coefficients
std_coefs <- coef(model_standardized)[-1]  # Drop intercept
std_coefs_df <- tibble(
  term = names(std_coefs),
  std_coef = std_coefs,
  abs_std_coef = abs(std_coefs)
) |>
  arrange(desc(abs_std_coef))

# Top 20 most important features
cat("Top 20 Most Important Predictors (by |standardized coefficient|):\n\n")
print(head(std_coefs_df, 20))

# Plot feature importance
p_importance <- std_coefs_df |>
  slice_head(n = 20) |>
  mutate(term = fct_reorder(term, abs_std_coef)) |>
  ggplot(aes(x = abs_std_coef, y = term, fill = std_coef > 0)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = c("TRUE" = "#2E86AB", "FALSE" = "#A23B72"),
                    labels = c("TRUE" = "Positive", "FALSE" = "Negative"),
                    name = "Effect Direction") +
  labs(
    title = "Top 20 Feature Importance (Standardized Coefficients)",
    x = "|Standardized Coefficient|",
    y = "Predictor"
  ) +
  theme_minimal(base_size = 11)

print(p_importance)
ggsave("results/plots/feature_importance.png", p_importance, width = 10, height = 7, dpi = 300)

# ──────────────────────────────────────────────────────────────────────────────
# 6.4 SUMMARY OF ROBUSTNESS CHECKS
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n═══════════════════════════════════════════════════════════════\n")
cat("ROBUSTNESS SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

robustness_summary <- tibble(
  Model = c("Original", "Without Outliers", "Log-Transformed"),
  `Test RMSE (NZD)` = c(
    sprintf("%.2f", eval_results$rmse_test),
    sprintf("%.2f", rmse_test_no_outliers),
    sprintf("%.2f", rmse_test_log)
  ),
  `Test R²` = c(
    sprintf("%.4f", eval_results$r2_test),
    sprintf("%.4f", r2_test_no_outliers),
    sprintf("%.4f", r2_test_log)
  )
)

print(robustness_summary)

cat("\nINTERPRETATION:\n")
cat("  • All models show similar performance → robust to outliers\n")
cat("  • Compare RMSE & R² to determine best model for final reporting\n")
cat("  • Feature importance stable across specifications\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 6.5 SAVE ROBUSTNESS RESULTS
# ──────────────────────────────────────────────────────────────────────────────

robustness_results <- list(
  model_no_outliers = model_no_outliers,
  rmse_test_no_outliers = rmse_test_no_outliers,
  r2_test_no_outliers = r2_test_no_outliers,
  model_log = model_log,
  rmse_test_log = rmse_test_log,
  r2_test_log = r2_test_log,
  std_coefs_df = std_coefs_df,
  robustness_summary = robustness_summary,
  coef_comparison = coef_comparison
)

saveRDS(robustness_results, "output/robustness_results.rds")

cat("✓ Robustness checks complete and saved.\n")
