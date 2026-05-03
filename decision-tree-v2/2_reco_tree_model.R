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
  data = df |> select(
    -response_id,
    -visit_purpose,
    -country_of_residence_group
  ),
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
# 1.846


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


########################
# poster
########################


label_map <- c(
  "activity_other_natural_attraction_e_g_mountain_lake_river_forest_etc" = "Natural attraction",
  "accomm_staying_with_family_or_friends" = "Staying with family/friends",
  "itinerary_region_otago" = "Visited Otago",
  "departure_location" = "Departure location",
  "gender" = "Gender",
  "activity_went_for_a_walk_hike_trek_or_tramp" = "Walked/hiked",
  "activity_a_place_that_is_significant_to_maori_such_as_a_landmark_remains_of_a_maori_pa_fortified_hill_etc" = "Māori significant place",
  "no_days_in_nz" = "Days in NZ",
  "share_cost_shopping" = "Shopping spend share",
  "maori_exp_none_of_these" = "No Māori experiences",
  "share_cost_food_drink" = "Food & drink spend share",
  "share_cost_entertainment" = "Entertainment spend share",
  "itinerary_region_wellington" = "Visited Wellington"
)

plot_tree_truncated <- function(model, max_depth = 5, label_map = NULL, ls = 150, ns = 200) {
  font <- "Arial"

  vt <- visTree(model, width = "100%", height = "1280", legend = FALSE)

  nodes <- vt$x$nodes
  edges <- vt$x$edges

  keep_nodes <- nodes[nodes$level <= max_depth, ]
  keep_ids <- keep_nodes$id
  keep_edges <- edges[edges$from %in% keep_ids & edges$to %in% keep_ids, ]

  keep_edges$label <- stringr::str_extract_all(keep_edges$title, "(?<=>)([^<]+)(?=<)") |>
    lapply(\(x) paste(x[-1], collapse = "\n")) |>
    unlist()

  keep_nodes$font.size <- 20
  keep_edges$font.size <- 12

  frame <- model$frame
  frame$node_id <- as.integer(rownames(frame))

  is_bottom <- keep_nodes$level == max_depth
  bottom_ids <- keep_nodes$id[is_bottom]

  for (nid in bottom_ids) {
    if (nid %in% frame$node_id) {
      mean_val <- round(frame$yval[frame$node_id == nid], 3)
      keep_nodes$label[keep_nodes$id == nid] <- as.character(mean_val)
    }
  }

  leaf_color <- "#9B59B6"

  keep_nodes$shape[is_bottom] <- "square"
  keep_nodes$Leaf[is_bottom] <- TRUE
  keep_nodes$color[is_bottom] <- leaf_color

  keep_nodes$color[keep_nodes$Leaf == TRUE] <- leaf_color

  if (!is.null(label_map)) {
    keep_nodes$label <- dplyr::recode(keep_nodes$label, !!!label_map)
  }

  visNetwork(
    nodes = keep_nodes, edges = keep_edges,
    width = "100%", height = "1280"
  ) |>
    visNodes(
      font = list(size = 20, face = font, background = "white"),
      widthConstraint = list(minimum = 200, maximum = 300)
    ) |>
    visEdges(
      font = list(size = 12, face = font, align = "top", background = "white"),
      smooth = list(enabled = TRUE)
    ) |>
    visHierarchicalLayout(direction = "UD", levelSeparation = ls, nodeSpacing = ns) |>
    visInteraction(tooltipStyle = glue(
      "position: fixed; visibility: hidden; padding: 10px;
    background-color: white; border: 1px solid #ccc; border-radius: 4px;
    font-family: {font}, serif; font-size: 13px; color: #333;"
    ))
}

p <- plot_tree_truncated(
  tree_pruned,
  max_depth = 5,
  label_map = label_map,
  ls = 175,
  ns = 200
)

saveWidget(p, "results/decision_tree_trunc.html", selfcontained = FALSE)
