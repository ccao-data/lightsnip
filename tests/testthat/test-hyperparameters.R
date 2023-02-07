context("test hyperparameters")

model <- parsnip::boost_tree() %>%
  parsnip::set_mode("regression")

# Test typical regression vars
param_df <- tibble::tribble(
  ~"param", ~"values",
  "max_bin", c(2, 4, 100),
  "max_depth", c(3, 6, 17),
  "learning_rate", c(0.001, 0.01, 0.1),
  "feature_fraction", c(0.1, 0.5, 1.0),
  "feature_fraction_bynode", c(0.1, 0.5, 1.0),
  "min_gain_to_split", c(0, 0.1, 10),
  "num_leaves", c(5, 10, 100),
  "lambda_l1", c(0, 0.5, 10),
  "lambda_l2", c(0, 0.5, 10)
)

purrr::pwalk(param_df, function(...) {
  hp <- list(...)

  test_that(paste("lightgbm", hp$param), {
    preds <- purrr::map2(hp$param, hp$values, function(x, y) {
      model <- model %>%
        parsnip::set_engine(
          engine = "lightgbm", verbose = -1, min_data_in_leaf = 1,
          {{ x }} := y
        )
      expect_regression_works(model)
      adj <- parsnip::fit(model, mpg ~ ., data = mtcars)
      predict(adj, mtcars)$.pred
    })
    expect_all_preds_differ(preds)
  })
})

test_that("lightgbm min_data_in_leaf", {
  min_data_in_leaf <- c(1, 5, 10)
  preds <- purrr::map(min_data_in_leaf, function(x) {
    model <- model %>%
      parsnip::set_engine(
        engine = "lightgbm", verbose = -1, min_data_in_leaf = x
      )
    expect_regression_works(model)
    adj <- parsnip::fit(model, mpg ~ ., data = mtcars)
    predict(adj, mtcars)$.pred
  })
  expect_all_preds_differ(preds)
})

# Special test for linking max_depth and num_leaves
test_that("lightgbm add_to_linked_depth", {
  add_to_linked_depth <- c(0, 3, 0, 3)
  num_leaves <- c(4, 4, 20, 20)
  preds <- purrr::map2(num_leaves, add_to_linked_depth, function(x, y) {
    model <- model %>%
      parsnip::set_engine(
        engine = "lightgbm", verbose = -1, min_data_in_leaf = 1,
        num_leaves = x, link_max_depth = TRUE, max_depth = -1,
        add_to_linked_depth = y
      )
    expect_regression_works(model)
    adj <- parsnip::fit(model, mpg ~ ., data = mtcars)
    predict(adj, mtcars)$.pred
    expect_add_to_linked_depth_works(adj, x, y)
  })
  expect_all_preds_differ(preds)
})

test_that("lightgbm bagging", {
  bagging_fraction <- c(0.3, 0.3, 0.5, 0.5)
  bagging_freq <- c(1, 50, 1, 50)
  preds <- purrr::map2(bagging_fraction, bagging_freq, function(x, y) {
    model <- model %>%
      parsnip::set_engine(
        engine = "lightgbm", verbose = -1, min_data_in_leaf = 5,
        bagging_fraction = x, bagging_freq = y
      ) %>%
      parsnip::set_args(trees = 100)
    expect_regression_works(model)
    adj <- parsnip::fit(model, mpg ~ ., data = mtcars)
    predict(adj, mtcars)$.pred
  })
  expect_all_preds_differ(preds)
})

# Test categorical-specific vars
param_df_cat <- tibble::tribble(
  ~"param", ~"values",
  "min_data_per_group", c(1, 50),
  "max_cat_threshold", c(5, 20),
  "cat_smooth", c(0.01, 10),
  "cat_l2", c(0.1, 100)
)

purrr::pwalk(param_df_cat, function(...) {
  hp <- list(...)

  test_that(paste("lightgbm", hp$param), {
    df <- tibble::tibble(
      y = runif(1000),
      x1 = as.factor(sample(letters, 1000, replace = TRUE))
    )

    preds <- purrr::map2(hp$param, hp$values, function(x, y) {
      model <- model %>%
        parsnip::set_engine(
          engine = "lightgbm", verbose = -1L, min_data_in_leaf = 1,
          {{ x }} := y,
          categorical_feature = "x1",
          seed = 27, deterministic = TRUE
        )
      expect_categorical_vars_works(model, df)
    })
    expect_all_preds_differ(preds)
  })
})
