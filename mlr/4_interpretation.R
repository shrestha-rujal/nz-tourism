library(tidyverse)

df <- read_csv("output/ols_coefficients_full.csv")

coef_ranked <- df |>
  filter(term != "(Intercept)" & p_value < 0.05) |>
  select(term, estimate, p_value, dollar_effect)

top_10_positive <- coef_ranked |>
  arrange(desc(estimate)) |>
  slice_head(n = 10)

top_10_negative <- coef_ranked |>
  arrange(estimate) |>
  slice_head(n = 10)

cat("\nTop 10 Positive Coefficients\n")
print(top_10_positive)

cat("\nTop 10 Negative Coefficients\n")
print(top_10_negative)
