#' Custom LightGBM objective: MSE with a squared covariance penalty
#'
#' @description Build a custom LightGBM objective callback that minimizes a
#' standard squared-error loss plus a soft penalty on the covariance between
#' the per-sample residual \code{r = y_pred - y_true} and the (centered) labels
#' `y_true`. The penalty pushes the model toward "vertical equity" by
#' discouraging residuals that systematically scale with `y`.
#'
#' This is an R port of the `LGBCovPenalty` objective from an active
#' collaboration (see the
# nolint start: line_length_linter.
#' [soft-vertical-equity-constrained-mass-appraissal](https://github.com/nicacevedo/soft-vertical-equity-constrained-mass-appraissal)
# nolint end
#' repository). It is intended to be used when the model is trained in
#' log-space (so the "diff" residual is equivalent to a log-ratio).
#'
#' Penalty (using mean-centered labels `y_centered = y_true - mean(y_true)`):
#' \deqn{\mathrm{cov} = \frac{1}{n} \sum_{i=1}^{n} r_i \tilde{y}_i}{
#'   cov = (1/n) * sum_i r_i * y_centered_i}
#' \deqn{\mathrm{penalty} = \frac{\rho}{2} \, n \, \mathrm{cov}^2}{
#'   penalty = 0.5 * rho * n * cov^2}
#' where \eqn{\tilde{y}}{y_centered} is the vector of mean-centered labels.
#' A diagonal Hessian approximation is used (matches the reference Python
#' implementation).
#'
#' @param rho Numeric. Non-negative penalty weight. `rho = 0` recovers plain
#'   MSE.
#' @param y_mean Numeric. Mean of the training labels. Should be computed once
#'   from the training set and captured here so the centering is stable across
#'   iterations.
#'
#' @return A function with signature `function(preds, dtrain)` suitable for
#'   passing as the `obj` argument of [lightgbm::lgb.train].
#'
#' @export
make_objective_mse_cov <- function(rho, y_mean) {
  rho <- as.numeric(rho)
  y_mean <- as.numeric(y_mean)
  if (length(rho) != 1L || is.na(rho) || rho < 0) {
    rlang::abort("`rho` must be a single non-negative numeric value.")
  }
  if (length(y_mean) != 1L || is.na(y_mean)) {
    rlang::abort("`y_mean` must be a single non-missing numeric value.")
  }

  function(preds, dtrain) {
    y_true <- lightgbm::get_field(dtrain, "label")
    y_pred <- as.numeric(preds)
    n <- length(y_pred)

    # Centered labels (training-set mean is captured at construction time so
    # the penalty geometry stays stable across boosting iterations)
    y_centered <- y_true - y_mean

    # Residual ("diff" mode); in log-space training this is the log-ratio
    r <- y_pred - y_true

    # Covariance estimate (E[y_centered] is ~0 by construction)
    cov_val <- mean(r * y_centered)

    # Base squared-error grad/hess
    grad_base <- 2.0 * (y_pred - y_true)
    hess_base <- rep(2.0, n)

    # Penalty grad/hess (diagonal approximation)
    # dc/dy_pred_i = (1/n) * y_centered_i  (since dr_i/dy_pred_i = 1)
    a <- y_centered / n
    grad_pen <- rho * n * cov_val * a
    hess_pen <- rho * n * (a^2)

    grad <- grad_base + grad_pen
    hess <- hess_base + hess_pen

    list(grad = grad, hess = hess)
  }
}
