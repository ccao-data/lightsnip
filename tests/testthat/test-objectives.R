construct_dtrain <- function(y) {
  dtrain <- lightgbm::lgb.Dataset(
    data = matrix(seq_len(length(y) * 2), ncol = 2),
    label = y,
    params = list(min_data_in_bin = 1)
  )
  lightgbm::lgb.Dataset.construct(dtrain)
  dtrain
}

test_that("mse_cov with rho = 0 recovers plain MSE grad/hess", {
  y <- as.numeric(1:20)
  preds <- y + seq(-2, 2, length.out = 20)
  dtrain <- construct_dtrain(y)

  obj <- make_objective_mse_cov(rho = 0, y_mean = mean(y))
  out <- obj(preds, dtrain)

  expect_equal(out$grad, 2 * (preds - y))
  expect_equal(out$hess, rep(2, length(y)))
})

test_that("mse_cov grad/hess match hand-computed penalty", {
  rho <- 0.5
  y <- as.numeric(1:20)
  preds <- y + seq(-2, 2, length.out = 20)
  dtrain <- construct_dtrain(y)

  obj <- make_objective_mse_cov(rho = rho, y_mean = mean(y))
  out <- obj(preds, dtrain)

  n <- length(y)
  y_centered <- y - mean(y)
  cov_val <- mean((preds - y) * y_centered)
  a <- y_centered / n

  expect_equal(out$grad, 2 * (preds - y) + rho * n * cov_val * a)
  expect_equal(out$hess, rep(2, n) + rho * n * a^2)
})

test_that("mse_cov validates rho", {
  expect_error(
    make_objective_mse_cov(rho = -1, y_mean = 0),
    regexp = "`rho` must be"
  )
  expect_error(
    make_objective_mse_cov(rho = c(1, 2), y_mean = 0),
    regexp = "`rho` must be"
  )
  expect_error(
    make_objective_mse_cov(rho = NA_real_, y_mean = 0),
    regexp = "`rho` must be"
  )
  expect_error(
    make_objective_mse_cov(rho = NULL, y_mean = 0),
    regexp = "`rho` must be"
  )
})

test_that("mse_cov validates y_mean", {
  expect_error(
    make_objective_mse_cov(rho = 1, y_mean = NA_real_),
    regexp = "`y_mean` must be"
  )
  expect_error(
    make_objective_mse_cov(rho = 1, y_mean = NULL),
    regexp = "`y_mean` must be"
  )
  expect_error(
    make_objective_mse_cov(rho = 1, y_mean = c(0, 1)),
    regexp = "`y_mean` must be"
  )
})
