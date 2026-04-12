library(tidyverse)
library(car)

setwd("/Users/rujalshrestha/Projects/nz-tourism/linear-regression")

# ══════════════════════════════════════════════════════════════════════════════
# PHASE 5: MODEL DIAGNOSTICS
# ══════════════════════════════════════════════════════════════════════════════

# Load data
model <- readRDS("output/fitted_model.rds")
data_list <- readRDS("output/train_test_data.rds")
X_train <- data_list$X_train
y_train <- data_list$y_train

cat("═══════════════════════════════════════════════════════════════\n")
cat("MODEL DIAGNOSTICS (Training Data)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 4.1 RESIDUAL ANALYSIS
# ──────────────────────────────────────────────────────────────────────────────

residuals <- residuals(model)
fitted_vals <- fitted(model)

cat("RESIDUAL SUMMARY:\n")
cat("─────────────────────────────────────────────────────────────\n")
print(summary(residuals))

# Residuals vs Fitted Values (heteroscedasticity)
p1 <- tibble(fitted = fitted_vals, resid = residuals) |>
  ggplot(aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red", linewidth = 1) +
  geom_smooth(method = "loess", colour = "blue", fill = "blue", alpha = 0.15) +
  labs(
    title = "Residuals vs Fitted Values",
    subtitle = "Check for heteroscedasticity (funnel shape = problem)",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 11)

print(p1)

# ──────────────────────────────────────────────────────────────────────────────
# 4.2 Q-Q PLOT (Normality of Residuals)
# ──────────────────────────────────────────────────────────────────────────────

p2 <- tibble(sample = qqnorm(residuals, plot.it = FALSE)$x,
             theoretical = qqnorm(residuals, plot.it = FALSE)$y) |>
  ggplot(aes(sample = residuals)) +
  geom_qq(alpha = 0.5, size = 1) +
  geom_qq_line(colour = "red", linewidth = 1) +
  labs(
    title = "Q-Q Plot",
    subtitle = "Check for normality of residuals (points along line = good)",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 11)

print(p2)

# ──────────────────────────────────────────────────────────────────────────────
# 4.3 SCALE-LOCATION PLOT (Homoscedasticity)
# ──────────────────────────────────────────────────────────────────────────────

sqrt_abs_resid <- sqrt(abs(residuals))

p3 <- tibble(fitted = fitted_vals, sqrt_resid = sqrt_abs_resid) |>
  ggplot(aes(x = fitted, y = sqrt_resid)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", colour = "blue", fill = "blue", alpha = 0.15) +
  labs(
    title = "Scale-Location Plot",
    subtitle = "Check for constant variance (horizontal line = good)",
    x = "Fitted Values",
    y = "√|Standardized Residuals|"
  ) +
  theme_minimal(base_size = 11)

print(p3)

# ──────────────────────────────────────────────────────────────────────────────
# 4.4 RESIDUALS DISTRIBUTION
# ──────────────────────────────────────────────────────────────────────────────

p4 <- tibble(resid = residuals) |>
  ggplot(aes(x = resid)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_density(aes(y = after_stat(count)), fill = "red", alpha = 0.3) +
  labs(
    title = "Distribution of Residuals",
    subtitle = "Should be approximately normal",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 11)

print(p4)

# ──────────────────────────────────────────────────────────────────────────────
# 4.5 CHECK MLR ASSUMPTIONS
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\nCHECKING MLR ASSUMPTIONS:\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# 1. Linearity - check if there are obvious non-linear patterns
cat("1. LINEARITY:\n")
cat("   Check residuals vs fitted plot above for patterns.\n")
cat("   If smooth curve is roughly horizontal → linearity OK\n\n")

# 2. Normality - Shapiro-Wilk test (use sample if n > 5000)
if (length(residuals) > 5000) {
  sample_residuals <- sample(residuals, 5000)
} else {
  sample_residuals <- residuals
}

shapiro_test <- shapiro.test(sample_residuals)
cat("2. NORMALITY (Shapiro-Wilk test):\n")
cat(sprintf("   Test statistic: %.4f\n", shapiro_test$statistic))
cat(sprintf("   p-value: %.4f\n", shapiro_test$p.value))
if (shapiro_test$p.value < 0.05) {
  cat("   ⚠️  Residuals significantly non-normal (p < 0.05)\n")
  cat("      Consider log-transforming outcome variable\n")
} else {
  cat("   ✓ Residuals approximately normal\n")
}
cat("\n")

# 3. Homoscedasticity - Breusch-Pagan test
cat("3. HOMOSCEDASTICITY (Breusch-Pagan test):\n")
bp_test <- lmtest::bptest(model)
cat(sprintf("   Test statistic: %.4f\n", bp_test$statistic))
cat(sprintf("   p-value: %.4f\n", bp_test$p.value))
if (bp_test$p.value < 0.05) {
  cat("   ⚠️  Heteroscedasticity detected (p < 0.05)\n")
  cat("      Consider robust standard errors or transformation\n")
} else {
  cat("   ✓ Constant variance assumption holds\n")
}
cat("\n")

# ──────────────────────────────────────────────────────────────────────────────
# 4.6 OUTLIERS & INFLUENTIAL POINTS
# ──────────────────────────────────────────────────────────────────────────────

cat("\n4. OUTLIERS & INFLUENTIAL POINTS:\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Cook's distance
cooks_d <- cooks.distance(model)

# Identify influential points (threshold: 4/n)
influence_threshold <- 4 / nrow(X_train)
influential_idx <- which(cooks_d > influence_threshold)

cat(sprintf("Cook's Distance threshold: %.6f\n", influence_threshold))
cat(sprintf("Number of influential points: %d (%.1f%%)\n",
           length(influential_idx),
           length(influential_idx) / nrow(X_train) * 100))

if (length(influential_idx) > 0) {
  cat("\nTop 10 most influential observations:\n")
  top_influential <- sort(cooks_d, decreasing = TRUE)[1:10]
  print(head(top_influential, 10))
}

# Plot Cook's distance
p5 <- tibble(obs = 1:length(cooks_d), cooks = cooks_d) |>
  ggplot(aes(x = obs, y = cooks)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = influence_threshold, linetype = "dashed", colour = "red", linewidth = 1) +
  labs(
    title = "Cook's Distance",
    subtitle = "Points above red line are influential",
    x = "Observation",
    y = "Cook's Distance"
  ) +
  theme_minimal(base_size = 11)

print(p5)

# ──────────────────────────────────────────────────────────────────────────────
# 4.7 SAVE DIAGNOSTICS
# ──────────────────────────────────────────────────────────────────────────────

diagnostics <- list(
  residuals = residuals,
  fitted_values = fitted_vals,
  cooks_distance = cooks_d,
  influential_idx = influential_idx,
  shapiro_test = shapiro_test,
  bp_test = bp_test
)

saveRDS(diagnostics, "output/diagnostics.rds")

cat("\n\n✓ Diagnostics complete and saved.\n")
