# nolint start
mtcars_class <- mtcars
mtcars_class$cyl <- as.factor(mtcars$cyl)
mtcars_class_binary <- mtcars
mtcars_class_binary$vs <- as.factor(mtcars$vs)

expect_all_modes_works <- function(model, engine) {
  if (engine == "lightgbm") {
    model <- parsnip::set_engine(model, engine, verbosity = -1L)
  } else {
    model <- parsnip::set_engine(model, engine)
  }

  expect_regression_works(parsnip::set_mode(model, "regression"))
  expect_categorical_vars_works(parsnip::set_mode(model, "regression"))
}

expect_regression_works <- function(model) {
  adj <- parsnip::fit(model, mpg ~ ., data = mtcars)

  pred <- predict(adj, mtcars)
  expect_equal(nrow(pred), nrow(mtcars))

  expect_mse(pred, mtcars$mpg, less_than = mean(mtcars$mpg))
  expect_not_constant_predictions(pred$.pred)

  # test if model can saved
  f <- tempfile(fileext = ".zip")
  lgbm_save(adj, f)

  rm(adj)
  gc()

  mod <- lgbm_load(f)
  expect_equal(predict(mod, mtcars), pred)
}

expect_categorical_vars_works <- function(model) {
  df <- data.frame(
    x1 = as.factor(sample(letters, 1000, replace = TRUE)),
    y = runif(1000)
  )

  adj <- parsnip::fit(model, y ~ ., data = df)

  p <- predict(adj, df)

  expect_true(length(unique(p$.pred)) <= length(unique(df$x1)))

  expect_error(
    predict(adj, data.frame(x1 = c("str", "str2"), stringsAsFactors = FALSE))
  )

  expect_equal(
    predict(
      adj,
      data.frame(x1 = factor(c("a", "b"), levels = c("b", "a")))
    )$.pred,
    predict(
      adj,
      data.frame(x1 = factor(c("a", "b"), levels = c("a", "b")))
    )$.pred
  )
}

expect_can_tune_boost_tree <- function(model) {
  mtcars <- dplyr::sample_n(mtcars, size = 500, replace = TRUE)

  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$vs <- factor(mtcars$vs)

  resamples <- rsample::vfold_cv(mtcars, v = 2)

  # regression
  adj <- tune::tune_grid(
    parsnip::set_mode(model, "regression"),
    mpg ~ .,
    resamples = resamples,
    grid = 2,
    metrics = yardstick::metric_set(yardstick::rmse)
  )

  expect_equal(nrow(adj), nrow(resamples))
  expect_equal(nrow(tune::collect_metrics(adj)), 2)
  expect_true(all(!is.nan(tune::collect_metrics(adj)$mean)))
}

expect_mse <- function(pred, true, less_than) {
  mse <- sqrt(mean((pred$.pred - true)^2))
  expect_true(mse < less_than)
}

expect_accuracy <- function(pred, true, at_least) {
  x <- table(pred, true)
  expect_true(sum(diag(x)) / sum(x) > at_least)
}

expect_not_constant_predictions <- function(pred) {
  expect_true(length(unique(pred)) > 1)
}

expect_between_0_and_1 <- function(pred) {
  expect_true(max(pred) <= 1)
  expect_true(min(pred) >= 0)
}

# nolint end
