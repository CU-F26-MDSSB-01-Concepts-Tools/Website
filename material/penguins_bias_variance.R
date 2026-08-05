# Load necessary libraries
library(tidymodels)
library(palmerpenguins)
library(dplyr)

# Load and clean the Palmer Penguins dataset
data <- penguins |>
  na.omit() |>
  select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm, species)

# Specify a Decision Tree model
tree_spec <- decision_tree(cost_complexity = 0) |>
  set_engine("rpart") |>
  set_mode("regression")

# Create a 5-fold cross-validation object
set.seed(1)
cv_folds <- vfold_cv(data, v = 10, , repeats = 20)

# Define a workflow
tree_wf <- workflow() |>
  add_model(tree_spec) |>
  add_formula(body_mass_g ~ .)

# Fit the model using cross-validation
cv_results <- fit_resamples(
  tree_wf,
  resamples = cv_folds,
  control = control_resamples(save_pred = TRUE) # we also want the predictions
)

# Collect predictions of all 5 repeats of 10-times cross-validations in one dataframe
cv_predictions <- collect_predictions(cv_results) # These are 20 times number of penguins rows

# Now we compute bias (squared collective error), variance, and average individual error for each prediction
# We have 20 predictions per original row

# Calculate the expected prediction (mean prediction for each observation)
bias_var_error <- cv_predictions |>
  mutate(
    # Compute some measures for each repeated observation
    mean_pred = mean(.pred), # mean prediction per original row
    true_val = body_mass_g,
    squared_diff = (.pred - mean_pred)^2, # use to compute variance per original row
    .by = .row
  ) |>
  summarize(
    # Summarize for each original value
    bias_squared = mean((mean_pred - true_val)^2),
    variance = mean(squared_diff),
    individual_error = mean((true_val - .pred)^2),
    .by = .row
  )

bias_var_error |>
  summarize(across(c(bias_squared, variance, individual_error), mean))


## Simulation with increasing cost_complexity
# Strategy: Put all of the above into a function which outputs bias square, variance, and individual error

bias_variance_error <- function(compl) {
  tree_spec <- decision_tree(cost_complexity = compl) |>
    set_engine("rpart") |>
    set_mode("regression")

  # Create a 5-fold cross-validation object
  set.seed(1)
  cv_folds <- vfold_cv(data, v = 10, , repeats = 20)

  # Define a workflow
  tree_wf <- workflow() |>
    add_model(tree_spec) |>
    add_formula(body_mass_g ~ .)

  # Fit the model using cross-validation
  cv_results <- fit_resamples(
    tree_wf,
    resamples = cv_folds,
    control = control_resamples(save_pred = TRUE) # we also want the predictions
  )

  # Collect predictions of all 5 repeats of 10-times cross-validations in one dataframe
  cv_predictions <- collect_predictions(cv_results) # These are 20 times number of penguins rows

  # Now we compute bias (squared collective error), variance, and average individual error for each prediction
  # We have 20 predictions per original row

  # Calculate the expected prediction (mean prediction for each observation)
  bias_var_error <- cv_predictions |>
    mutate(
      # Compute some measures for each repeated observation
      mean_pred = mean(.pred), # mean prediction per original row
      true_val = body_mass_g,
      squared_diff = (.pred - mean_pred)^2, # use to compute variance per original row
      .by = .row
    ) |>
    summarize(
      # Summarize for each original value
      bias_squared = mean((mean_pred - true_val)^2),
      variance = mean(squared_diff),
      individual_error = mean((true_val - .pred)^2),
      .by = .row
    )

  bias_var_error |>
    summarize(across(c(bias_squared, variance, individual_error), mean)) |>
    mutate(cost_complexity = compl)
}

simulation <- seq(0, 0.015, by = 0.001) |>
  map(\(x) bias_variance_error(x)) |>
  reduce(bind_rows)
simulation |>
  pivot_longer(c(bias_squared, variance, individual_error)) |>
  ggplot(aes(x = cost_complexity, y = value, color = name)) +
  geom_line() +
  geom_point() +
  scale_x_reverse() +
  facet_wrap(~name, ncol = 1, scales = "free_y")
