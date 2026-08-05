library(tidyverse)
library(tidymodels)
library(palmerpenguins)

# Compare a logistic regression and a decision tree

## Specification of penguins sex classification model with logistic regression and a decision tree
peng_recipe <- recipe(sex ~ ., data = penguins)
peng_logreg <- logistic_reg() |>
  set_engine("glm")
peng_tree <- decision_tree() |>
  set_engine("rpart") |>
  set_mode("classification")

## Train and test data
penguins_split <- initial_split(penguins, prop = 0.7, strata = sex)
peng_train <- training(penguins_split)
peng_test <- training(penguins_split)

## Fit both models
peng_logreg_fit <- fit(
  workflow() |>
    add_recipe(peng_recipe) |>
    add_model(peng_logreg),
  data = peng_train
)
peng_tree_fit <- fit(
  workflow() |>
    add_recipe(peng_recipe) |>
    add_model(peng_tree),
  data = peng_train
)

## Look at both models
peng_logreg_fit |> tidy()
peng_tree_fit
peng_tree_fit$fit$fit$fit |> rpart.rules(style = "tall", roundint = FALSE)
peng_tree_fit$fit$fit$fit |> rpart.plot(roundint = FALSE)


## Make predictions for test data
peng_logreg_pred <- predict(peng_logreg_fit, peng_test) |> bind_cols(peng_test)
peng_tree_pred <- predict(peng_tree_fit, peng_test) |> bind_cols(peng_test)

## Print the confusion matrix
peng_logreg_pred |> conf_mat(truth = sex, estimate = .pred_class)
peng_tree_pred |> conf_mat(truth = sex, estimate = .pred_class)


## p-values of linear models

set.seed(1)
penguins_split <- initial_split(penguins, prop = 0.7, strata = sex)
peng_train <- training(penguins_split)
peng_test <- testing(penguins_split)
peng_workflow <- workflow() |> add_recipe(peng_recipe)
peng_logreg_fit <- peng_workflow |>
  add_model(peng_logreg) |>
  fit(data = peng_train)
peng_logreg_fit |> tidy()

b <- bootstraps(penguins, times = 2000, apparent = TRUE)

peng_fit_lm <- function(split) {
  glm(sex ~ ., family = binomial, data = analysis(split))
}
peng_fit_lm(b$splits[[1]]) |> tidy()
b_models <-
  b |> mutate(model = map(splits, peng_fit_lm), coef_info = map(model, tidy))
b_models

b_coefs <-
  b_models %>%
  unnest(coef_info)

b_coefs |>
  filter(term == "flipper_length_mm") |>
  select(estimate) |>
  count(estimate > 0) |>
  mutate(p = n / 2000)
b_coefs |>
  filter(term == "flipper_length_mm") |>
  ggplot(aes(x = p.value)) +
  geom_histogram()

b_coefs |>
  filter(term == "speciesGentoo") |>
  ggplot(aes(x = estimate)) +
  geom_histogram()
b_coefs |>
  filter(term == "speciesGentoo") |>
  ggplot(aes(x = p.value)) +
  geom_histogram()
b_coefs |>
  filter(term == "speciesChinstrap") |>
  ggplot(aes(x = estimate)) +
  geom_histogram()
b_coefs |>
  filter(term == "speciesGentoo") |>
  ggplot(aes(x = p.value)) +
  geom_histogram()

b_coefs |>
  filter(term == "body_mass_g") |>
  ggplot(aes(x = estimate)) +
  geom_histogram()
b_coefs |>
  filter(term == "body_mass_g") |>
  ggplot(aes(x = estimate)) +
  geom_histogram()
b_coefs |>
  filter(term == "(Intercept)") |>
  ggplot(aes(x = estimate)) +
  geom_histogram()


ggplot(aes(x = estimate)) +
  geom_histogram(bins = 30) +
  labs(
    x = "Flipper length (mm)",
    y = "Count",
    title = "Distribution of flipper length"
  ) +
  theme_minimal()


# Random forest

penguins_forest <- rand_forest(trees = 500, mtry = 7) %>%
  set_engine("ranger") %>%
  set_mode("classification")

penguins_forest_workflow <- workflow() |>
  add_recipe(penguins_recipe) |>
  add_model(penguins_forest)

penguins_forest_fit <- fit(penguins_forest_workflow, data = penguins_train)

penguins_forest_pred <- predict(penguins_forest_fit, penguins_test) |>
  bind_cols(penguins_test)
penguins_forest_pred_train <- predict(penguins_forest_fit, penguins_train) |>
  bind_cols(penguins_train)

# Print the evaluation metrics
penguins_forest_pred |> conf_mat(truth = sex, estimate = .pred_class)
penguins_forest_pred |>
  conf_mat(truth = sex, estimate = .pred_class) |>
  summary() |>
  slice(1:4)
penguins_forest_pred_train |>
  conf_mat(truth = sex, estimate = .pred_class) |>
  summary() |>
  slice(1:4)
