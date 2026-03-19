library(tidyverse)
library(rpart)
library(rpart.plot)
library(visNetwork)
library(sparkline)
library(htmlwidgets)
library(glue)

plot_tree <- function(model) {
  font <- "Arial"

  visTree(model, width = "100%", height = "900px", legend = FALSE) |>
    visNodes(
      font = list(size = 12, face = font),
      widthConstraint = list(minimum = 200, maximum = 300)
    ) |>
    visEdges(font = list(size = 10, face = font)) |>
    visHierarchicalLayout(direction = "UD", levelSeparation = 150, nodeSpacing = 200) |>
    visInteraction(tooltipStyle = glue(
      "position: fixed; visibility: hidden; padding: 10px;
      background-color: white; border: 1px solid #ccc; border-radius: 4px;
      font-family: {font}, serif; font-size: 13px; color: #333;"
    ))
}

df <- readRDS("output/merged/reco_tree_model_data.rds")

# hotfix
# @TODO: move to step 1 file
df <- df |>
  mutate(country_of_residence = fct_lump_min(country_of_residence, min = 10))


set.seed(123)

tree_model_full <- rpart(recommend_rating ~ .,
  data = df |> select(-response_id),
  method = "anova",
  control = rpart.control(cp = 0.0001, xval = 10)
)

# find optimal cp
cp_table <- tree_model_full$cptable
min_xerror_row <- which.min(cp_table[, "xerror"])
threshold <- cp_table[min_xerror_row, "xerror"] + cp_table[min_xerror_row, "xstd"]
optimal_cp <- cp_table[cp_table[, "xerror"] <= threshold, "CP"] |> max()
cat("Optimal cp:", optimal_cp, "\n")
# 0.004490052

# prune
tree_pruned <- prune(tree_model_full, cp = optimal_cp)

#####################
# PLOTTINGS
#####################

# plot cp chart
# png("results/plots/cost_parameter_plot.png", width = 1200, height = 600)
# plotcp(tree_model_full)
# dev.off()

# plot tree
p <- plot_tree(tree_pruned)
# saveWidget(p, "results/plots/decision_tree.html", selfcontained = FALSE)

#####################
# MSE
#####################

# use full data for the final model (stable cp)
# use train/test split just to report MSE
set.seed(123)
train_idx <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[train_idx, ] |> select(-response_id)
test_data <- df[-train_idx, ] |> select(-response_id)

tree_test <- prune(
  rpart(recommend_rating ~ .,
    data = train_data,
    method = "anova", control = rpart.control(cp = optimal_cp)
  ),
  cp = optimal_cp
)

tree_preds <- predict(tree_test, test_data)
mse <- mean((test_data$recommend_rating - tree_preds)^2)
cat("Test MSE:", mse, "\n")
# 1.3998


##########################################
# ANALYSIS OF PREDICTORS
##########################################

importance_df <- data.frame(
  variable = names(tree_pruned$variable.importance),
  importance = tree_pruned$variable.importance
) |>
  arrange(desc(importance))

p_importance <- ggplot(
  importance_df |> filter(importance >= 1),
  aes(x = reorder(variable, importance), y = importance)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  # scale_y_log10() +
  labs(
    title = "Variable Importance from Pruned Decision Tree",
    x = "Variable",
    y = "Importance (log scale)"
  ) +
  theme_minimal(base_size = 16)

# ggsave("results/plots/variable_importance.jpg", dpi = 300)
