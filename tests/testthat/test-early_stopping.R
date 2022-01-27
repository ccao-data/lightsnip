context("lightgbm early stopping")

test_that("validation sets", {
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 100, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = .1,
        verbose = -1, min_data_in_leaf = 1, num_leaves = 50
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars[- (1:4), ]),
    regex = NA
  )

  expect_equal(names(reg_fit$fit$record_evals)[2], "validation")

  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 20, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = .1, metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 50
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars[- (1:4), ]),
    regex = NA
  )

  expect_equal(names(reg_fit$fit$record_evals$validation)[1], "rmse")

  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 20, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 50
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars[- (1:4), ]),
    regex = NA
  )

  expect_equal(names(reg_fit$fit$record_evals)[2], "training")

  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 20, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = 3, metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 50
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars[- (1:4), ]),
    regex = "`validation` should be on"
  )

})

test_that("early stopping", {
  set.seed(233456)
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 1000, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = .1, metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 20,
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars[- (1:4), ]),
    regex = NA
  )

  expect_true(reg_fit$fit$params$num_iterations > reg_fit$fit$best_iter)

  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 1000, mode = "regression", stop_iter = 2) %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = .1, metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 20,
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars[- (1:4), ]),
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
      parsnip::set_engine("lightgbm", validation = .1, verbose = -1) %>%
      parsnip::fit(mpg ~ ., data = mtcars[- (1:4), ]),
    regex = "`early_stop` was reduced to 19"
  )
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 20, stop_iter = 0, mode = "regression") %>%
      parsnip::set_engine("lightgbm", validation = .1, verbose = -1) %>%
      parsnip::fit(mpg ~ ., data = mtcars[- (1:4), ]),
    regex = "`early_stop` should be on"
  )
})

test_that("early stopping with weights", {
  set.seed(233456)
  expect_error(
    reg_fit <-
      parsnip::boost_tree(trees = 1000, mode = "regression") %>%
      parsnip::set_engine(
        engine = "lightgbm", validation = .1, metric = "rmse",
        verbose = -1, min_data_in_leaf = 1, num_leaves = 20,
        weight = rep(c(0.5, 1), nrow( mtcars[- (1:4), ]) / 2)
      ) %>%
      parsnip::fit(mpg ~ ., data = mtcars[- (1:4), ]),
    regex = NA
  )

  expect_true(reg_fit$fit$params$num_iterations > reg_fit$fit$best_iter)
})
