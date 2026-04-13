library(car)
library(tidyverse)

df <- readRDS("output/model_df.rds")

set.seed(42)

n <- nrow(df)
train_idx <- sample.int(n, size = floor(0.8 * n), replace = FALSE)

train_df <- df[train_idx, ]
test_df <- df[-train_idx, ]

ols_full <- lm(log_treated_spend ~ ., data = train_df)

raw_vif <- car::vif(ols_full)

vif_tbl <- if (is.matrix(raw_vif)) {
  tibble(
    term = rownames(raw_vif),
    vif = raw_vif[, "GVIF"]^(1 / (2 * raw_vif[, "Df"]))
  )
} else {
  tibble(
    term = names(raw_vif),
    vif = as.numeric(raw_vif)
  )
}

vif_tbl |>
  arrange(desc(vif)) |>
  print(n = 50)
