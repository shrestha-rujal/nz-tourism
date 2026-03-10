library(tidyverse)
library(rpart)
library(visNetwork)
library(sparkline)

df <- readRDS("output/merged/satisfaction_tree_model_data.rds")

# hotfix
# @TODO: move to step 1 file
df <- df |>
  mutate(country_of_residence = fct_lump_min(country_of_residence, min = 10))

set.seed(123)
n <- nrow(df)
train_idx <- sample(1:n, size = 0.8 * n)

train_data <- df[train_idx, ] |> select(-response_id)
test_data <- df[-train_idx, ] |> select(-response_id)

tree_model <- rpart(satisfaction_rating ~ .,
  data = train_data,
  method = "anova",
  control = rpart.control(cp = 0.01)
)

tree_preds <- predict(tree_model, test_data)
tree_mse <- mean((test_data$satisfaction_rating - tree_preds)^2)
cat("Decision Tree MSE:", tree_mse, "\n")

visTree(tree_model, width = "100%", height = "900px") |>
  visNodes(font = list(size = 12), widthConstraint = list(minimum = 200, maximum = 300)) |>
  visEdges(font = list(size = 10)) |>
  visHierarchicalLayout(direction = "UD", levelSeparation = 150, nodeSpacing = 200)
