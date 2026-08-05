library(tidymodels)
library(openintro)
library(tidyverse)
library(rpart.plot)


# Email spam-prediction: Overfitting a decision tree

email_recipe <- recipe(spam ~ ., data = email) |>
  step_rm(from, sent_email, viagra) |>
  step_date(time, features = c("dow", "month")) |>
  step_rm(time) |>
  step_dummy(all_nominal(), -all_outcomes()) |>
  step_zv(all_predictors())

email_tree <- decision_tree(
  cost_complexity = -1,
  tree_depth = 30
) |>
  set_engine("rpart") |>
  set_mode("classification")

email_workflow <- workflow() |>
  add_recipe(email_recipe) |>
  add_model(email_tree)

set.seed(123)
email_split <- initial_split(email, prop = 0.7, strata = spam)
email_train <- training(email_split)
email_test <- testing(email_split)

# Train the model
email_fit <- fit(email_workflow, data = email_train)
email_fit
email_fit$fit$fit$fit |>
  rpart.plot(roundint = FALSE, tweak = 0.5)

# Make predictions
email_pred_test <- predict(email_fit, email_test) |>
  bind_cols(email_test)
email_pred_train <- predict(email_fit, email_train) |>
  bind_cols(email_train)

# Print the evaluation metrics
email_pred_test |> conf_mat(truth = spam, estimate = .pred_class)
email_pred_train |> conf_mat(truth = spam, estimate = .pred_class)
email_pred_test |> conf_mat(truth = spam, estimate = .pred_class) |> summary()
# Select accuracy, sensitivity, specificity
email_pred_test |>
  conf_mat(truth = spam, estimate = .pred_class) |>
  summary() |>
  slice(c(1, 3, 4))
email_pred_train |>
  conf_mat(truth = spam, estimate = .pred_class) |>
  summary() |>
  slice(c(1, 3, 4))


# Function to get accuracy of a decision tree by complexity cost for test and train data
decisiontree_fit_predict_accuracy_by_complexitycost <- function(compl) {
  data_fit <- fit(
    workflow() |>
      add_recipe(email_recipe) |>
      add_model(
        decision_tree(cost_complexity = compl, tree_depth = 30, min_n = 1) |>
          set_engine("rpart") |>
          set_mode("classification")
      ),
    data = email_train
  )
  data_pred_test <- predict(data_fit, email_test) |> bind_cols(email_test)
  data_pred_train <- predict(data_fit, email_train) |> bind_cols(email_train)
  bind_rows(
    data_pred_test |>
      accuracy(truth = spam, estimate = .pred_class) |>
      mutate(data = "test"),
    data_pred_train |>
      accuracy(truth = spam, estimate = .pred_class) |>
      mutate(data = "train")
  )
}

# Test the function
decisiontree_fit_predict_accuracy_by_complexitycost(0.05) |>
  filter(.metric == "accuracy")
decisiontree_fit_predict_accuracy_by_complexitycost(0.02) |>
  filter(.metric == "accuracy")
decisiontree_fit_predict_accuracy_by_complexitycost(0.01) |>
  filter(.metric == "accuracy")
decisiontree_fit_predict_accuracy_by_complexitycost(0.005) |>
  filter(.metric == "accuracy")
decisiontree_fit_predict_accuracy_by_complexitycost(0.002) |>
  filter(.metric == "accuracy")
decisiontree_fit_predict_accuracy_by_complexitycost(0.001) |>
  filter(.metric == "accuracy")
decisiontree_fit_predict_accuracy_by_complexitycost(0) |>
  filter(.metric == "accuracy")

# Collect values in a dataframe
simtest <- seq(0, 0.02, 0.001) |>
  map_df(\(x) {
    decisiontree_fit_predict_accuracy_by_complexitycost(x) |>
      mutate(complexitycost = x)
  })

# Show the overfitting in a plot
simtest |>
  ggplot(aes(x = complexitycost, y = .estimate, color = data)) +
  geom_line() +
  geom_point() +
  scale_x_reverse() +
  labs(x = "Cost complexity (-> Less pruning)", y = "Accuracy")


# Random forrest

email_forest <- rand_forest(trees = 500, mtry = 6) %>%
  set_engine("ranger") %>%
  set_mode("classification")
email_forest_workflow <- workflow() |>
  add_recipe(email_recipe) |>
  add_model(email_forest)
email_forest_fit <- fit(email_forest_workflow, data = email_train)
email_forest_pred <- predict(email_forest_fit, email_test) |>
  bind_cols(email_test)
email_forest_pred |> conf_mat(truth = spam, estimate = .pred_class)
email_forest_pred |>
  conf_mat(truth = spam, estimate = .pred_class) |>
  summary() |>
  slice(c(1, 3, 4))

# For comparison a well pruned decision tree
fit(
  workflow() |>
    add_recipe(email_recipe) |>
    add_model(
      decision_tree(cost_complexity = 0.005, tree_depth = 30, min_n = 1) |>
        set_engine("rpart") |>
        set_mode("classification")
    ),
  data = email_train
) |>
  predict(new_data = email_test) |>
  bind_cols(email_test) |>
  conf_mat(truth = spam, estimate = .pred_class) |>
  summary() |>
  slice(c(1, 3, 4))

# Tune the random forest with more trees and more features per tree
workflow() |>
  add_recipe(email_recipe) |>
  add_model(
    rand_forest(trees = 2500, mtry = 9) %>%
      set_engine("ranger") %>%
      set_mode("classification")
  ) |>
  fit(data = email_train) |>
  predict(new_data = email_test) |>
  bind_cols(email_test) |>
  conf_mat(truth = spam, estimate = .pred_class) |>
  summary() |>
  slice(c(1, 3, 4))


# For comparison logistic regression
workflow() |>
  add_recipe(email_recipe) |>
  add_model(
    logistic_reg() |>
      set_engine("glm")
  ) |>
  fit(data = email_train) |>
  predict(new_data = email_test) |>
  bind_cols(email_test) |>
  conf_mat(truth = spam, estimate = .pred_class) |>
  summary() |>
  slice(c(1, 3, 4))
