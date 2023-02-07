context("test early stopping")

test_that("validation uses correct dimensions", {
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 100, stop_iter = 10, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 0.25,
        verbose = -1, min_data_in_leaf = 1, num_leaves = 50
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = NA
  )

  expect_equal(names(reg_fit$fit$record_evals)[2], "validation")
  expect_equal(
    reg_fit$fit$.__enclos_env__$private$valid_sets[[1]]$dim(),
    c(8, 10)
  )
  expect_equal(
    reg_fit$fit$.__enclos_env__$private$train_set$dim(),
    c(24, 10)
  )
})

test_that("train and validation do not overlap", {
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 100, stop_iter = 10, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 0.25, sample_type = "random",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 50,
        free_raw_data = FALSE
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = NA
  )

  trn_set <- reg_fit$fit$.__enclos_env__$private$train_set$
    .__enclos_env__$private$raw_data
  val_set <- reg_fit$fit$.__enclos_env__$private$valid_sets[[1]]$
    .__enclos_env__$private$raw_data

  expect_equal(
    nrow(dplyr::semi_join(as.data.frame(trn_set), as.data.frame(val_set))),
    0
  )
})

test_that("validation sample type gets last X percent", {
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 100, stop_iter = 10, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 0.25, sample_type = "recent",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 50,
        free_raw_data = FALSE
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars %>% dplyr::arrange(disp)),
    regex = NA
  )

  val_set <- reg_fit$fit$.__enclos_env__$private$valid_sets[[1]]$
    .__enclos_env__$private$raw_data
  mtcars_sorted <- mtcars %>%
    dplyr::slice_max(disp, prop = 0.25) %>%
    dplyr::arrange(disp) %>%
    dplyr::select(-mpg) %>%
    as.matrix()

  expect_identical(val_set, mtcars_sorted)
})

test_that("validation sample type changes results", {
  expect_error(
    reg_fit_random <-
      parsnip::boost_tree(trees = 100, stop_iter = 10, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 0.25, sample_type = "random",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 50
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = NA
  )
  expect_error(
    reg_fit_recent <-
      parsnip::boost_tree(trees = 100, stop_iter = 10, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 0.25, sample_type = "recent",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 50
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = NA
  )
  expect_equal(
    reg_fit_random$fit$.__enclos_env__$private$valid_sets[[1]]$dim(),
    reg_fit_recent$fit$.__enclos_env__$private$valid_sets[[1]]$dim()
  )
  pred_random <- predict(reg_fit_random, mtcars)$.pred
  pred_recent <- predict(reg_fit_recent, mtcars)$.pred
  expect_all_preds_differ(list(pred_random, pred_recent))
})

test_that("validation sets can use alternate metric", {
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 20, stop_iter = 10, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 0.25, metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 50
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = NA
  )

  expect_equal(names(reg_fit$fit$record_evals$validation)[1], "rmse")
})

test_that("validation set uses proportion", {
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 20, stop_iter = 3, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 3, metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 50
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = "`validation` should be on"
  )
})

test_that("early stopping works", {
  set.seed(233456)
  expect_error(
    reg_fit <-
      parsnip::boost_tree(
        trees = 1000,
        stop_iter = 10,
        mode = "regression"
      ) %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 0.25, metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 20,
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = NA
  )

  expect_true(reg_fit$fit$params$num_iterations > reg_fit$fit$best_iter)

  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 1000, mode = "regression", stop_iter = 2) %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 0.1, metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 20,
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = NA
  )

  expect_equal(
    length(reg_fit$fit$record_evals$validation$rmse$eval),
    reg_fit$fit$best_iter + 2
  )
  expect_true(
    length(reg_fit$fit$record_evals$validation$rmse$eval) <
      reg_fit$fit$params$num_iterations
  )

  expect_warning(
    reg_fit <-
      parsnip::boost_tree(trees = 20, stop_iter = 30, mode = "regression") %>%
      parsnip::set_engine("lightgbm", validation = 0.1, verbose = -1) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = "`early_stop` is greater than"
  )
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 20, stop_iter = 0, mode = "regression") %>%
      parsnip::set_engine("lightgbm", validation = .1, verbose = -1) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = "`early_stop` should be on"
  )
})

test_that("early stopping with weights", {
  set.seed(233456)
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 1000, stop_iter = 3, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 0.1, metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 20,
        weight = rep(c(0.5, 1), nrow(mtcars) / 2)
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = NA
  )

  expect_true(reg_fit$fit$params$num_iterations > reg_fit$fit$best_iter)
})

test_that("lgbm_save uses best prediction", {
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 1000, stop_iter = 10, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 0.1, metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 20
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars),
    regex = NA
  )

  best_iter <- reg_fit$fit$best_iter
  max_iter <- length(reg_fit$fit$record_evals$validation$rmse$eval)

  f <- tempfile()
  lightgbm::lgb.save(reg_fit$fit, f)

  reg_fit_loaded <- lightgbm::lgb.load(f)

  expect_equal(best_iter, reg_fit_loaded$current_iter())
  expect_gt(max_iter, reg_fit_loaded$current_iter())
})
