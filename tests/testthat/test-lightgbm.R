context("test lightgbm regression")

test_that("lightgbm regression", {
  model <- parsnip::boost_tree(trees = 50) %>%
    parsnip::set_engine(
      engine = "lightgbm",
      objective = "regression", verbose = -1,
      max_depth = 15, feature_fraction = 1, min_data_in_leaf = 1
    ) %>%
    parsnip::set_mode("regression")

  expect_regression_works(model)
})

test_that("lightgbm with weights", {
  model <- parsnip::boost_tree(trees = 50) %>%
    parsnip::set_engine(
      engine = "lightgbm",
      objective = "regression", verbose = -1,
      max_depth = 15, feature_fraction = 1, min_data_in_leaf = 1,
      weight = rep(1, nrow(mtcars))
    ) %>%
    parsnip::set_mode("regression")

  expect_regression_works(model)

  model_w_weights <- parsnip::boost_tree(trees = 50) %>%
    parsnip::set_engine(
      engine = "lightgbm",
      objective = "regression", verbose = -1,
      max_depth = 15, feature_fraction = 1, min_data_in_leaf = 1,
      weight = rep(c(0.5, 1), nrow(mtcars) / 2)
    ) %>%
    parsnip::set_mode("regression")

  expect_regression_works(model_w_weights)

  # Do weighted pred differ from unweighted?
  adj <- parsnip::fit(model, mpg ~ ., data = mtcars)
  adj_w <- parsnip::fit(model_w_weights, mpg ~ ., data = mtcars)

  pred <- predict(adj, mtcars)$.pred
  pred_w <- predict(adj_w, mtcars)$.pred
  expect_all_preds_differ(list(pred, pred_w))
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

test_that("lightgbm with save_tree_error saves record_evals", {
  model_fit <- parsnip::boost_tree(trees = 50) %>%
    parsnip::set_engine(
      engine = "lightgbm",
      objective = "regression", metric = "rmse", verbose = -1,
      max_depth = 15, feature_fraction = 1, min_data_in_leaf = 1,
      save_tree_error = TRUE
    ) %>%
    parsnip::set_mode("regression") %>%
    parsnip::fit(mpg ~ ., data = mtcars)

  expect_equal(
    length(model_fit$fit$record_evals$tree_errors$rmse$eval),
    50
  )
})

test_that("lightgbm with save_tree_error and validation throws error", {
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 50, stop_iter = 2, mode = "regression") %>%
      parsnip::set_engine(
        "lightgbm",
        save_tree_error = TRUE, validation = 0.25
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = "`save_tree_error` cannot be `TRUE`"
  )
})

test_that("lighgbm throws error", {
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 40, stop_iter = 2, mode = "regression") %>%
      parsnip::set_engine("lightgbm", validation = 10, verbose = -1) %>%
      parsnip::fit(mpg ~ ., data = mtcars[-(1:4), ]),
    regex = "`validation` should be on"
  )
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
