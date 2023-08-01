
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Lightsnip <a href='https://github.com/ccao-data/lightsnip'><img src='man/figures/logo.png' align="right" height="139" /></a>

[![R-CMD-check](https://github.com/ccao-data/lightsnip/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ccao-data/lightsnip/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/ccao-data/lightsnip/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/ccao-data/lightsnip/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/ccao-data/lightsnip/actions/workflows/lint.yaml/badge.svg)](https://github.com/ccao-data/lightsnip/actions/workflows/lint.yaml)
[![pre-commit](https://github.com/ccao-data/lightsnip/actions/workflows/pre-commit.yaml/badge.svg)](https://github.com/ccao-data/lightsnip/actions/workflows/pre-commit.yaml)
[![codecov](https://codecov.io/gh/ccao-data/lightsnip/branch/master/graph/badge.svg)](https://codecov.io/gh/ccao-data/lightsnip)

Lightsnip is a hard fork of
[curso-r/treesnip](https://github.com/curso-r/treesnip). It adds
LightGBM bindings for parsnip and enables more advanced LightGBM
features, such as early stopping. It is not intended for general use,
only as a dependency for CCAO regression models.

For detailed documentation on included functions, [**visit the full
reference
list**](https://ccao-data.github.io/lightsnip/reference/index.html).

## Installation

You can install the released version of `lightsnip` directly from GitHub
with one of the following commands:

``` r
# Using remotes
remotes::install_github("ccao-data/lightsnip")

# Using renv
renv::install("ccao-data/lightsnip")

# Using pak
pak::pak("ccao-data/lightsnip")

# Append the @ symbol for a specific version
remotes::install_github("ccao-data/lightsnip@0.0.5")
```

Once it is installed, you can use it just like any other package. Simply
call `library(assessr)` at the beginning of your script.

## Differences compared to [treesnip](https://github.com/curso-r/treesnip)

- Removed support for `tree` and `catboost` (LightGBM only)
- Removed classification support for LightGBM (regression only)
- Removed treesnip caps and warnings on `max_depth`, other parameters
- Removed vignettes and samples
- Remap parameters to engine args instead of parsnip model args
- Added LightGBM-specific hyperparameter functions
- Added LightGBM-specific save/load helpers
- Added recipe/fit cleaning helpers
- Force user to specify categorical columns by name, does *not*
  implicitly convert factors to categoricals
- Added early stopping from xgboost
- Added more unit tests
- Fixed a number of bugs

## Basic usage with Tidymodels

Here is a quick example using `lightsnip` with a Tidymodels
cross-validation workflow:

``` r
library(dplyr)
library(lightgbm)
library(lightsnip)
library(parsnip)
library(recipes)
library(workflows)

# Create a dataset for training
mtcars_train <- mtcars %>%
  dplyr::slice(1:28) %>%
  sample_n(size = 500, replace = TRUE) %>%
  mutate(cyl = as.factor(cyl), vs = as.factor(vs))

# Create a test set
mtcars_test <- mtcars %>%
  dplyr::slice(29:32) %>%
  mutate(cyl = as.factor(cyl), vs = as.factor(vs))

# Recipe to convert factors to categorical integers
rec <- recipe(mpg ~ ., mtcars_train) %>%
  step_integer(all_nominal(), zero_based = TRUE)

# Split data into V-folds
resamples <- rsample::vfold_cv(mtcars_train, v = 2)

# Create a model specification. LightGBM-specific parameters are passed to
# set_engine, NOT to boost_tree
model <- parsnip::boost_tree(
  trees = tune::tune()
) %>%
  parsnip::set_engine(
    engine = "lightgbm",
    verbose = -1,
    learning_rate = tune::tune(),
    min_gain_to_split = tune::tune(),
    feature_fraction = tune::tune(),
    min_data_in_leaf = tune::tune(),
    max_depth = tune::tune()
  )

# Run grid search
search <- tune::tune_grid(
  parsnip::set_mode(model, "regression"),
  preprocessor = rec,
  resamples = resamples,
  param_info = model %>%
    hardhat::extract_parameter_set_dials() %>%
    stats::update(
      learning_rate = learning_rate(),
      min_gain_to_split = min_gain_to_split(),
      feature_fraction = feature_fraction(),
      min_data_in_leaf = min_data_in_leaf(c(1L, 2L)),
      max_depth = max_depth(c(3L, 6L))
    ),
  grid = 2,
  metrics = yardstick::metric_set(yardstick::rmse)
)

# Finalize model
final <- model %>%
  tune::finalize_model(tune::select_best(search)) %>%
  parsnip::set_mode("regression") %>%
  parsnip::fit(mpg ~ ., bake(prep(rec), mtcars_train))

# Predict on test set
mtcars_test %>%
  mutate(pred_mpg = predict(final, bake(prep(rec), .))$.pred) %>%
  select(actual_mpg = mpg, pred_mpg) %>%
  knitr::kable(digits = 2)
```

|                | actual_mpg | pred_mpg |
|:---------------|-----------:|---------:|
| Ford Pantera L |       15.8 |    14.08 |
| Ferrari Dino   |       19.7 |    21.20 |
| Maserati Bora  |       15.0 |    13.72 |
| Volvo 142E     |       21.4 |    22.12 |
