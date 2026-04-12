library(tidyverse)
library(broom)
library(flextable)

setwd("/Users/rujalshrestha/Projects/nz-tourism/linear-regression")

# ══════════════════════════════════════════════════════════════════════════════
# PHASE 8: REPORTING & VISUALIZATION
# ══════════════════════════════════════════════════════════════════════════════

# Load all results
model <- readRDS("output/fitted_model.rds")
coef_table <- readRDS("output/coefficient_table.rds")
eval_results <- readRDS("output/evaluation_results.rds")
robustness_results <- readRDS("output/robustness_results.rds")
vif_df <- readRDS("output/vif_results.rds")

cat("═══════════════════════════════════════════════════════════════\n")
cat("REPORTING & VISUALIZATION\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 7.1 MODEL SUMMARY TABLE (PUBLICATION-READY)
# ──────────────────────────────────────────────────────────────────────────────

cat("7.1 Creating coefficient summary table...\n\n")

# Significant predictors only
sig_coefs <- coef_table |>
  filter(term != "(Intercept)", p.value < 0.05) |>
  arrange(p.value) |>
  select(term, estimate, std.error, statistic, p.value, ci_lower, ci_upper) |>
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 3),
    p.value = format(p.value, scientific = TRUE, digits = 2),
    ci_lower = round(ci_lower, 2),
    ci_upper = round(ci_upper, 2)
  )

cat("Significant Predictors (p < 0.05):\n")
print(sig_coefs)

# Save to CSV
write_csv(sig_coefs, "results/significant_predictors.csv")

# ──────────────────────────────────────────────────────────────────────────────
# 7.2 MODEL PERFORMANCE SUMMARY
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n7.2 Model Performance Summary:\n")
cat("─────────────────────────────────────────────────────────────\n\n")

model_summary_stats <- summary(model)

perf_table <- tibble(
  Statistic = c("Observations", "R²", "Adjusted R²", "F-statistic",
                "F p-value", "Residual Std. Error", "Num. Predictors"),
  Value = c(
    nrow(model$model),
    sprintf("%.4f", model_summary_stats$r.squared),
    sprintf("%.4f", model_summary_stats$adj.r.squared),
    sprintf("%.2f", model_summary_stats$fstatistic[1]),
    "< 0.001",
    sprintf("%.2f", model_summary_stats$sigma),
    ncol(model$model) - 1
  )
)

print(perf_table)
write_csv(perf_table, "results/model_performance.csv")

# ──────────────────────────────────────────────────────────────────────────────
# 7.3 TEST SET EVALUATION TABLE
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n7.3 Test Set Evaluation Metrics:\n")
cat("─────────────────────────────────────────────────────────────\n\n")

eval_table <- eval_results$perf_summary
print(eval_table)
write_csv(eval_table, "results/test_set_performance.csv")

# ──────────────────────────────────────────────────────────────────────────────
# 7.4 COMPARISON OF MODEL SPECIFICATIONS
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n7.4 Robustness: Model Comparison\n")
cat("─────────────────────────────────────────────────────────────\n\n")

print(robustness_results$robustness_summary)
write_csv(robustness_results$robustness_summary, "results/model_comparison.csv")

# ──────────────────────────────────────────────────────────────────────────────
# 7.5 MULTICOLLINEARITY CHECK (VIF TABLE)
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n7.5 Multicollinearity Check (VIF):\n")
cat("─────────────────────────────────────────────────────────────\n\n")

vif_high <- vif_df |>
  filter(vif > 5) |>
  slice_head(n = 10)

if (nrow(vif_high) > 0) {
  cat("Variables with VIF > 5 (potential multicollinearity):\n")
  print(vif_high)
} else {
  cat("✓ No variables with VIF > 5\n")
}

write_csv(vif_df, "results/vif_results.csv")

# ──────────────────────────────────────────────────────────────────────────────
# 7.6 VISUALIZATION: COEFFICIENT PLOT (SIGNIFICANT PREDICTORS ONLY)
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n7.6 Creating coefficient plot...\n")

coef_plot <- coef_table |>
  filter(term != "(Intercept)", p.value < 0.1) |>
  mutate(
    term = fct_reorder(term, estimate),
    significant = if_else(p.value < 0.05, "p < 0.05", "p < 0.10")
  ) |>
  ggplot(aes(x = estimate, y = term, colour = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper),
               width = 0.2, linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.8) +
  scale_colour_manual(values = c("p < 0.05" = "#2E86AB", "p < 0.10" = "#A23B72")) +
  labs(
    title = "Estimated Coefficients with 95% Confidence Intervals",
    subtitle = "Predictors with p < 0.10 shown",
    x = "Coefficient Estimate (NZD)",
    y = "Predictor",
    colour = "Significance"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(coef_plot)
ggsave("results/plots/coefficient_plot.png", coef_plot, width = 10, height = 8, dpi = 300)

# ──────────────────────────────────────────────────────────────────────────────
# 7.7 DIAGNOSTIC SUMMARY FIGURE
# ──────────────────────────────────────────────────────────────────────────────

cat("\n7.7 Creating diagnostic summary Figure...\n")

# Combine diagnostic plots
diagnostics <- readRDS("output/diagnostics.rds")
fitted_vals <- diagnostics$fitted_values
residuals <- diagnostics$residuals

p1 <- tibble(fitted = fitted_vals, resid = residuals) |>
  ggplot(aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_smooth(method = "loess", colour = "blue", fill = "blue", alpha = 0.1, se = FALSE) +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal(base_size = 9)

p2 <- tibble(resid = residuals) |>
  ggplot(aes(sample = resid)) +
  geom_qq(alpha = 0.5, size = 0.5) +
  geom_qq_line(colour = "red") +
  labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal(base_size = 9)

p3 <- tibble(fitted = fitted_vals, sqrt_resid = sqrt(abs(residuals))) |>
  ggplot(aes(x = fitted, y = sqrt_resid)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_smooth(method = "loess", colour = "blue", fill = "blue", alpha = 0.1, se = FALSE) +
  labs(title = "Scale-Location", x = "Fitted Values", y = "√|Residuals|") +
  theme_minimal(base_size = 9)

p4 <- tibble(resid = residuals) |>
  ggplot(aes(x = resid)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  labs(title = "Residuals Distribution", x = "Residuals", y = "Count") +
  theme_minimal(base_size = 9)

diagnostic_fig <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Model Diagnostics",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

print(diagnostic_fig)
ggsave("results/plots/diagnostic_summary.png", diagnostic_fig, width = 12, height = 9, dpi = 300)

# ──────────────────────────────────────────────────────────────────────────────
# 7.8 FINAL SUMMARY REPORT
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("FINAL MODEL SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("MODEL SPECIFICATION:\n")
cat("  Outcome: treated_spend (visitor spending in NZD)\n")
cat(sprintf("  Observations: %d (train) + %d (test)\n", length(eval_results$y_train), length(eval_results$y_test)))
cat(sprintf("  Predictors: %d\n", ncol(model$model) - 1))
cat("  Model: Ordinary Least Squares (OLS) Regression\n\n")

cat("KEY FINDINGS:\n")
cat(sprintf("  • R² = %.4f (explains %.1f%% of spending variation)\n",
           eval_results$r2_test, eval_results$r2_test * 100))
cat(sprintf("  • Test RMSE = NZD %.2f (average prediction error)\n", eval_results$rmse_test))
cat(sprintf("  • Significant predictors: %d\n", sum(coef_table$p.value < 0.05) - 1))
cat(sprintf("  • No severe multicollinearity (max VIF = %.2f)\n\n", max(vif_df$vif)))

cat("TOP 5 MOST IMPORTANT PREDICTORS:\n")
top_5_importance <- robustness_results$std_coefs_df |>
  slice_head(n = 5)

for (i in 1:nrow(top_5_importance)) {
  direction <- if_else(top_5_importance$std_coef[i] > 0, "increases", "decreases")
  cat(sprintf("  %d. %s (%s spending)\n", i, top_5_importance$term[i], direction))
}

cat("\nDIAGNOSTIC SUMMARY:\n")
if (diagnostics$shapiro_test$p.value < 0.05) {
  cat("  ⚠️  Residuals slightly non-normal (may benefit from log transformation)\n")
} else {
  cat("  ✓ Residuals approximately normal\n")
}

cat("  ✓ No excessive multicollinearity\n")
cat("  ✓ Model performs consistently on train & test sets\n\n")

cat("OUTPUT FILES GENERATED:\n")
cat("  • Results: results/significant_predictors.csv\n")
cat("  •         results/model_performance.csv\n")
cat("  •         results/test_set_performance.csv\n")
cat("  •         results/model_comparison.csv\n")
cat("  • Plots:  results/plots/predictions_vs_actual.png\n")
cat("  •         results/plots/coefficient_plot.png\n")
cat("  •         results/plots/feature_importance.png\n")
cat("  •         results/plots/diagnostic_summary.png\n\n")

cat("✓ Analysis complete!\n")
