context("lightgbm regression")

test_that("lightgbm regression", {
  model <- parsnip::boost_tree(trees = 50) %>%
    parsnip::set_engine(
      engine = "lightgbm",
      objective = "regression", verbose = -1, verbosity = -1,
      max_depth = 15, feature_fraction = 1, min_data_in_leaf = 1
    ) %>%
    parsnip::set_mode("regression")

  expect_regression_works(model)
})

test_that("lightgbm with categoricals", {
  model <- parsnip::boost_tree(trees = 50) %>%
    parsnip::set_engine(
      engine = "lightgbm",
      objective = "regression", verbose = -1,
      max_depth = 15, feature_fraction = 1, min_data_in_leaf = 1,
      categorical_feature = "x1"
    ) %>%
    parsnip::set_mode("regression")

  df <- data.frame(
    x1 = as.factor(sort(c(sample(letters, 1000, replace = TRUE)))),
    y = c(runif(900), runif(100, 10, 20))
  )

  expect_categorical_vars_works(model, df)
})

test_that("lightgbm alternate objective", {
  skip_if_not_installed("lightgbm")

  spec <- parsnip::boost_tree(trees = 50) %>%
    parsnip::set_engine(
      engine = "lightgbm",
      objective = "huber", verbose = -1,
      max_depth = 15, feature_fraction = 1, min_data_in_leaf = 1
    ) %>%
    parsnip::set_mode("regression")

  lgb_fit <- spec %>% parsnip::fit(mpg ~ ., data = mtcars)

  predict(lgb_fit, mtcars[, -1])

  info <- jsonlite::fromJSON(lightgbm::lgb.dump(lgb_fit$fit))

  expect_equal(info$objective, "huber")
})

context("test tune")

test_that("lightgbm with tune", {
  model <- parsnip::boost_tree(
    trees = tune::tune()
  ) %>%
    parsnip::set_engine(
      engine = "lightgbm", verbose = -1,
      learning_rate = tune::tune(),
      min_gain_to_split = tune::tune(),
      feature_fraction = tune::tune(),
      min_data_in_leaf = tune::tune(),
      max_depth = tune::tune()
    )

  expect_can_tune_boost_tree(model)
})
