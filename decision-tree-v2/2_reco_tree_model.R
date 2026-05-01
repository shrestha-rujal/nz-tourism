library(tidyverse)
library(rpart)
library(rpart.plot)
library(visNetwork)
library(sparkline)
library(htmlwidgets)
library(glue)

plot_tree <- function(model) {
  font <- "Arial"

  visTree(model, width = "100%", height = "1280", legend = FALSE) |>
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

# setwd("/home/rujal/Projects/nz-tourism/decision-tree-v2")
df <- readRDS("output/dt2_modal.rds")

set.seed(123)

tree_model_full <- rpart(recommend_rating ~ .,
  data = df |> select(-response_id),
  method = "anova",
  control = rpart.control(cp = 0.0001, xval = 10)
)

# find optimal cp
cp_table <- tree_model_full$cptable
min_xerror_row <- which.min(cp_table[, "xerror"])
# threshold <- cp_table[min_xerror_row, "xerror"] + cp_table[min_xerror_row, "xstd"]
# optimal_cp <- cp_table[cp_table[, "xerror"] <= threshold, "CP"] |> max()
optimal_cp <- cp_table[min_xerror_row, "CP"]
optimal_cp

# prune
tree_pruned <- prune(tree_model_full, cp = 0.0012)

#####################
# PLOTTINGS
#####################

# plot cp chart
# png("results/plots/cost_parameter_plot.png", width = 1200, height = 600)
# plotcp(tree_model_full)
# dev.off()

# plot tree
p <- plot_tree(tree_pruned)

# saveWidget(p, "results/decision_tree.html", selfcontained = FALSE)

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
# 1.869


##########################################
# ANALYSIS OF PREDICTORS
##########################################

importance_df <- data.frame(
  variable = names(tree_pruned$variable.importance),
  importance = tree_pruned$variable.importance
) |>
  arrange(desc(importance)) |>
  slice_head(n = 20) |>
  mutate(
    variable = str_trunc(variable, width = 70)
  )

p_importance <- ggplot(
  importance_df,
  aes(x = reorder(variable, importance), y = importance)
) +
  geom_col(fill = "steelblue", width = 0.75) +
  coord_flip() +
  labs(
    title = "Top 20 Variable Importances (Pruned Decision Tree)",
    x = "Variable",
    y = "Importance"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold")
  )

p_importance

ggsave(
  filename = "results/variable_importance.jpg",
  plot = p_importance,
  width = 18,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)
