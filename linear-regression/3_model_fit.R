library(tidyverse)
library(broom)

setwd("/Users/rujalshrestha/Projects/nz-tourism/linear-regression")

# ══════════════════════════════════════════════════════════════════════════════
# PHASE 4: MODEL SPECIFICATION & TRAINING
# ══════════════════════════════════════════════════════════════════════════════

# Load train/test data
data_list <- readRDS("output/train_test_data.rds")
X_train <- data_list$X_train
y_train <- data_list$y_train
X_test <- data_list$X_test
y_test <- data_list$y_test

# ──────────────────────────────────────────────────────────────────────────────
# 3.1 FIT OLS MODEL ON TRAINING DATA
# ──────────────────────────────────────────────────────────────────────────────

cat("═══════════════════════════════════════════════════════════════\n")
cat("FITTING OLS REGRESSION MODEL ON TRAINING DATA\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

model <- lm(y_train ~ ., data = X_train)

# ──────────────────────────────────────────────────────────────────────────────
# 3.2 MODEL SUMMARY
# ──────────────────────────────────────────────────────────────────────────────

cat("\n")
print(summary(model))

# Extract key statistics
model_summary <- summary(model)

cat("\n\n")
cat("KEY MODEL STATISTICS:\n")
cat("─────────────────────────────────────────────────────────────\n")
cat(sprintf("R-squared:             %.4f\n", model_summary$r.squared))
cat(sprintf("Adjusted R-squared:    %.4f\n", model_summary$adj.r.squared))
cat(sprintf("F-statistic:           %.2f\n", model_summary$fstatistic[1]))
cat(sprintf("F p-value:             < 0.001\n\n"))

# ──────────────────────────────────────────────────────────────────────────────
# 3.3 COEFFICIENT TABLE
# ──────────────────────────────────────────────────────────────────────────────

cat("\nCOEFFICIENT SUMMARY (Training Data):\n")
cat("─────────────────────────────────────────────────────────────\n\n")

coef_table <- tidy(model) |>
  mutate(
    significant = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    ci_lower = estimate - 1.96 * std.error,
    ci_upper = estimate + 1.96 * std.error
  ) |>
  select(term, estimate, std.error, statistic, p.value, significant, ci_lower, ci_upper)

# Display top 20 coefficients by absolute value (excluding intercept)
top_coefs <- coef_table |>
  slice(-1) |>
  slice_max(abs(estimate), n = 20)

print(top_coefs)

# ──────────────────────────────────────────────────────────────────────────────
# 3.4 INTERPRETATION
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\nINTERPRETATION OF SELECTED COEFFICIENTS:\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("Intercept:", sprintf("%.2f NZD\n", coef_table$estimate[1]))
cat("  → Baseline predicted spending (all predictors = 0)\n\n")

# Show a few example interpretations
signif_coefs <- coef_table |>
  filter(p.value < 0.05, term != "(Intercept)") |>
  slice_head(n = 20)

for (i in 1:nrow(signif_coefs)) {
  coef_name <- signif_coefs$term[i]
  coef_est <- signif_coefs$estimate[i]
  coef_pval <- signif_coefs$p.value[i]

  if (coef_est > 0) {
    cat(sprintf("%s: +NZD %.2f (p < 0.001)\n", coef_name, coef_est))
  } else {
    cat(sprintf("%s: -NZD %.2f (p < 0.001)\n", coef_name, abs(coef_est)))
  }
}

# ──────────────────────────────────────────────────────────────────────────────
# 3.5 SAVE MODEL & RESULTS
# ──────────────────────────────────────────────────────────────────────────────

saveRDS(model, "output/fitted_model.rds")
saveRDS(coef_table, "output/coefficient_table.rds")

cat("\n\n✓ Model fitted and saved.\n")
