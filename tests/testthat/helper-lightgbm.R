# nolint start

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
    x1 = as.factor(c("a", "b", sample(letters, 998, replace = TRUE))),
    y = runif(1000)
  )

  rec <- recipes::recipe(y ~ ., df) %>%
    recipes::step_integer(recipes::all_nominal(), zero_based = TRUE)

  adj <- parsnip::fit(
    model, y ~ .,
    data = recipes::bake(recipes::prep(rec), df)
  )

  p <- predict(adj, recipes::bake(recipes::prep(rec), df))

  expect_true(length(unique(p$.pred)) <= length(unique(df$x1)))

  expect_error(
    predict(adj, data.frame(x1 = c("str", "str2"), stringsAsFactors = FALSE))
  )

  expect_equal(
    predict(
      adj,
      recipes::bake(
        recipes::prep(rec),
        data.frame(x1 = factor(c("a", "b"), levels = c("a", "b")))
      )
    )$.pred,
    predict(
      adj,
      recipes::bake(
        recipes::prep(rec),
        data.frame(x1 = factor(c("a", "b"), levels = c("b", "a")))
      )
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
    param_info = model %>%
      dials::parameters() %>%
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

  expect_equal(nrow(adj), nrow(resamples))
  expect_equal(nrow(tune::collect_metrics(adj)), 2)
  expect_true(all(!is.nan(tune::collect_metrics(adj)$mean)))

  final <- model %>%
    tune::finalize_model(tune::select_best(adj)) %>%
    parsnip::set_mode("regression") %>%
    parsnip::fit(mpg ~ ., mtcars)

  pred <- predict(final, mtcars)
  expect_equal(nrow(pred), nrow(mtcars))

  expect_mse(pred, mtcars$mpg, less_than = mean(mtcars$mpg))
  expect_not_constant_predictions(pred$.pred)
}

expect_mse <- function(pred, true, less_than) {
  mse <- sqrt(mean((pred$.pred - true)^2))
  expect_true(mse < less_than)
}

expect_not_constant_predictions <- function(pred) {
  expect_true(length(unique(pred)) > 1)
}

expect_all_preds_differ <- function(preds) {
  gr <- expand.grid(x = seq_len(length(preds)), y = seq_len(length(preds))) %>%
    dplyr::filter(.data$x != .data$y)
  predictions_differ <- any(
    purrr::map2_lgl(gr$x, gr$y, ~ isTRUE(all.equal(preds[[.x]], preds[[.y]])))
  )
  expect_false(predictions_differ)
}

# nolint end
