library(tidyverse)
library(rsample)
library(pROC)
library(broom)

setwd("/home/rujal/Projects/nz-tourism/logistic-regression")

#####################
# LOAD DATA
#####################

dt <- readRDS("../decision-tree-v2/output/dt2_modal.rds") |>
  select(-recommend_rating)

sat <- read_csv("../decision-tree/output/satisfaction_cleaned.csv",
  show_col_types = FALSE
) |>
  select(response_id, satisfaction_rating)

env <- read_csv("../decision-tree/output/environment_processed.csv",
  show_col_types = FALSE
)

#####################
# MERGE + PREP
#####################

merged <- dt |>
  left_join(sat, by = "response_id") |>
  left_join(env, by = "response_id") |>
  select(-any_of("expectation_rating")) |>
  mutate(satisfied = as.integer(satisfaction_rating >= 4)) |>
  select(-response_id, -satisfaction_rating) |>
  na.omit()

cat(sprintf("Rows after cleanup: %d | Columns: %d\n", nrow(merged), ncol(merged)))
cat(sprintf(
  "Outcome distribution — 0: %d | 1: %d | Prop satisfied: %.4f\n",
  sum(merged$satisfied == 0), sum(merged$satisfied == 1),
  mean(merged$satisfied)
))

#####################
# TRAIN/TEST SPLIT
#####################

set.seed(123)
split <- initial_split(merged, prop = 0.75, strata = "satisfied")
train <- training(split) |> mutate(across(where(is.character), as.factor))
test <- testing(split) |> mutate(across(where(is.character), as.factor))

cat(sprintf("Train: %d | Test: %d\n", nrow(train), nrow(test)))

#####################
# MODEL
#####################

model <- glm(satisfied ~ ., data = train, family = binomial)
cat("Model fitted.\n\n")

#####################
# EVALUATION
#####################

probs <- predict(model, newdata = test, type = "response")
roc_obj <- pROC::roc(test$satisfied, probs, quiet = TRUE)
auc_val <- pROC::auc(roc_obj)

best <- coords(roc_obj, "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)
opt_thresh <- best$threshold

eval_metrics <- function(preds, actuals) {
  tp <- sum(preds == 1 & actuals == 1)
  fp <- sum(preds == 1 & actuals == 0)
  fn <- sum(preds == 0 & actuals == 1)
  tn <- sum(preds == 0 & actuals == 0)
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  list(
    accuracy    = mean(preds == actuals),
    precision   = precision,
    recall      = recall,
    specificity = tn / (tn + fp),
    f1          = 2 * precision * recall / (precision + recall)
  )
}

m_default <- eval_metrics(as.integer(probs >= 0.5), test$satisfied)
m_optimal <- eval_metrics(as.integer(probs >= opt_thresh), test$satisfied)

cat("=== Threshold: 0.5 ===\n")
cat(sprintf(
  "Accuracy: %.4f | Precision: %.4f | Recall: %.4f | F1: %.4f\n",
  m_default$accuracy, m_default$precision,
  m_default$recall, m_default$f1
))
print(table(Predicted = as.integer(probs >= 0.5), Actual = test$satisfied))

cat(sprintf("\n=== Optimal Threshold (Youden's J): %.4f ===\n", opt_thresh))
cat(sprintf(
  "Sensitivity: %.4f | Specificity: %.4f\n",
  best$sensitivity, best$specificity
))
cat(sprintf(
  "Accuracy: %.4f | Precision: %.4f | Recall: %.4f | F1: %.4f\n",
  m_optimal$accuracy, m_optimal$precision,
  m_optimal$recall, m_optimal$f1
))
print(table(Predicted = as.integer(probs >= opt_thresh), Actual = test$satisfied))

cat(sprintf("\nAUC: %.4f\n", auc_val))

#####################
# COEFFICIENTS
#####################

coefs <- tidy(model) |>
  mutate(odds_ratio = exp(estimate)) |>
  filter(term != "(Intercept)", p.value < 0.05) |>
  arrange(desc(abs(estimate)))

cat("\n=== Significant Coefficients (p < 0.05) ===\n")
print(coefs, n = Inf)

#####################
# VISUALISATIONS
#####################

dir.create("results", showWarnings = FALSE)

# 1. Odds ratio plot
p_coef <- coefs |>
  head(20) |>
  mutate(
    direction = if_else(odds_ratio >= 1, "Increases odds", "Decreases odds"),
    term = str_replace(term, "age_range\\.L", "age_range (linear trend)")
  ) |>
  ggplot(aes(x = reorder(term, log(odds_ratio)), y = odds_ratio, fill = direction)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  scale_y_log10() +
  scale_fill_manual(values = c("Increases odds" = "#0072B2", "Decreases odds" = "#D55E00")) +
  coord_flip() +
  labs(
    title = "Significant Predictors of Visitor Satisfaction",
    subtitle = "Odds ratios from logistic regression (p < 0.05), log scale",
    x = NULL, y = "Odds Ratio (log scale)", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title       = element_text(face = "bold"),
    legend.position  = "bottom"
  )

print(p_coef)
ggsave("results/logit_coefficients.jpg", p_coef,
  width = 20, height = 7,
  dpi = 300, bg = "white"
)

# 2. ROC curve
roc_df <- data.frame(
  fpr = 1 - roc_obj$specificities,
  tpr = roc_obj$sensitivities
)

p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(colour = "#0072B2", linewidth = 1) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  annotate("text",
    x = 0.65, y = 0.15,
    label = sprintf("AUC = %.3f", as.numeric(auc_val)),
    size = 4.5, colour = "grey20"
  ) +
  labs(
    title = "ROC Curve — Visitor Satisfaction Model",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title       = element_text(face = "bold")
  )

print(p_roc)
ggsave("results/logit_roc.jpg", p_roc,
  width = 7, height = 6,
  dpi = 300, bg = "white"
)


library(knitr)

# Evaluation table
eval_table <- tibble(
  Metric = c("Accuracy", "Precision", "Recall (Sensitivity)", "Specificity", "F1 Score", "AUC"),
  `Threshold 0.5` = c(
    m_default$accuracy, m_default$precision, m_default$recall,
    m_default$specificity, m_default$f1, as.numeric(auc_val)
  ),
  `Optimal Threshold` = c(
    m_optimal$accuracy, m_optimal$precision, m_optimal$recall,
    m_optimal$specificity, m_optimal$f1, as.numeric(auc_val)
  )
) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n=== Evaluation Metrics ===\n")
print(knitr::kable(eval_table, format = "simple"))

# Confusion matrix table
cat("\n=== Confusion Matrix (Optimal Threshold) ===\n")
cm <- table(Predicted = as.integer(probs >= opt_thresh), Actual = test$satisfied)
rownames(cm) <- c("Predicted: Unsatisfied", "Predicted: Satisfied")
colnames(cm) <- c("Actual: Unsatisfied", "Actual: Satisfied")
print(knitr::kable(cm, format = "simple"))
