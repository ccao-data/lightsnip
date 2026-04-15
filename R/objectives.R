#' Custom LightGBM objective: MSE + rho * Cov(r, y)^2
#'
#' @description Build a custom LightGBM objective callback that minimizes a
#' standard squared-error loss plus a soft penalty on the covariance between
#' the per-sample residual `r = y_pred - y_true` and the (centered) labels
#' `y_true`. The penalty pushes the model toward "vertical equity" by
#' discouraging residuals that systematically scale with `y`.
#'
#' This is an R port of the `LGBCovPenalty` objective from
#' an active collaboration.
#' (https://github.com/nicacevedo/soft-vertical-equity-constrained-mass-appraissal)
#' It is intended to be used when the model is trained in log-space (so the
#' "diff" residual is equivalent to a log-ratio).
#'
#' Penalty (using mean-centered labels yc = y_true - mean(y_true)):
#' \deqn{cov = (1/n) * sum_i r_i * yc_i}
#' \deqn{penalty = 0.5 * rho * n * cov^2}
#' Diagonal Hessian approximation is used (matches the reference Python
#' implementation).
#'
#' @param rho Numeric. Non-negative penalty weight. `rho = 0` recovers plain
#'   MSE.
#' @param y_mean Numeric. Mean of the training labels. Should be computed once
#'   from the training set and captured here so the centering is stable across
#'   iterations.
#' @param zero_grad_tol Numeric. Floor applied to absolute gradients/Hessians
#'   to avoid zero entries that confuse LightGBM. Matches the reference
#'   implementation.
#'
#' @return A function with signature `function(preds, dtrain)` suitable for
#'   passing as the `obj` argument of [lightgbm::lgb.train].
#'
#' @export
make_obj_mse_cov <- function(rho, y_mean, zero_grad_tol = 1e-6) {
  rho <- as.numeric(rho)
  y_mean <- as.numeric(y_mean)
  zero_grad_tol <- as.numeric(zero_grad_tol)
  if (length(rho) != 1L || is.na(rho) || rho < 0) {
    rlang::abort("`rho` must be a single non-negative numeric value.")
  }

  function(preds, dtrain) {
    y_true <- lightgbm::get_field(dtrain, "label")
    y_pred <- as.numeric(preds)
    n <- length(y_pred)

    # Centered labels (training-set mean is captured at construction time so
    # the penalty geometry stays stable across boosting iterations)
    yc <- y_true - y_mean

    # Residual ("diff" mode); in log-space training this is the log-ratio
    r <- y_pred - y_true

    # Covariance estimate (E[yc] is ~0 by construction)
    cov_val <- mean(r * yc)

    # Base squared-error grad/hess
    grad_base <- 2.0 * (y_pred - y_true)
    hess_base <- rep(2.0, n)

    # Penalty grad/hess (diagonal approximation)
    # dc/dy_pred_i = (1/n) * yc_i  (since dr_i/dy_pred_i = 1)
    a <- yc / n
    grad_pen <- rho * n * cov_val * a
    hess_pen <- rho * n * (a^2)

    grad <- grad_base + grad_pen
    hess <- hess_base + hess_pen

    # Floor tiny values, mirroring the reference implementation
    small_g <- abs(grad) < zero_grad_tol
    if (any(small_g)) grad[small_g] <- zero_grad_tol
    small_h <- hess < zero_grad_tol
    if (any(small_h)) hess[small_h] <- zero_grad_tol

    list(grad = grad, hess = hess)
  }
}
