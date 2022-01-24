# nocov start
#' LightGBM engine-specific hyperparameters
#'
#' @description Collection of hyperparameters specific to LightGBM. Each can be
#' passed to \code{\link[parsnip]{set_engine}} for tuning. See the LightGBM
#' \href{https://lightgbm.readthedocs.io/en/latest/Parameters.html}{docs} for
#' more information on each parameter.
#'
#' @param range A two-element vector holding the defaults for the smallest and
#'   largest possible values, respectively.
#' @param trans A trans object from the \code{scales} package, such
#'   as \code{\link[scales]{log10_trans}} or
#'   \code{\link[scales]{reciprocal_trans}}. If not provided, the default is
#'   used which matches the units used in range. If no transformation, NULL.
#'
#' @examples
#' max_depth()
#' learning_rate()
#' feature_fraction()
#' lambda_l1()
#' lambda_l2()
#' cat_smooth()
#' @name param_lgbm
NULL


#' @rdname param_lgbm
#' @export
max_bin <- function(range = c(100L, 500L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(max_bin = "Max Bins"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
max_depth <- function(range = c(3L, 17L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(max_depth = "Max Tree Depth"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
add_to_linked_depth <- function(range = c(1L, 3L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(max_depth = "Add to floor(log2(num_leaves))"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
learning_rate <- function(range = c(-3, -0.5), trans = scales::log10_trans()) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(learning_rate = "Learning Rate"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
feature_fraction <- function(range = c(0.3, 1.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(feature_fraction = "% Features Selected by Iteration"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
feature_fraction_bynode <- function(range = c(0.3, 1.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(feature_fraction_bynode = "% Features Selected by Node"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
min_data_in_leaf <- function(range = c(20L, 500L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(min_data_in_leaf = "Minimal # of Data in One Leaf"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
min_gain_to_split <- function(range = c(-3, 2),
                              trans = scales::log10_trans()) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(min_gain_to_split = "Minimal Gain to Perform Split"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
num_leaves <- function(range = c(31L, 4000L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_leaves = "Max Number of Leaves in One Tree"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
bagging_fraction <- function(range = c(0.3, 1.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(bagging_fraction = "% Features Selected Without Resampling"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
bagging_freq <- function(range = c(1L, 100L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(bagging_freq = "Frequency of Bagging by Iteration"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
min_data_per_group <- function(range = c(20L, 250L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(min_data_per_group = "Minimal # of Data per Categorical Group"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
max_cat_threshold <- function(range = c(20L, 250L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(max_cat_threshold = "Max # of Split Points per Categorical Group"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
lambda_l1 <- function(range = c(-3, 2), trans = scales::log10_trans()) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(lambda_l1 = "L1 Regularization"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
lambda_l2 <- function(range = c(-3, 2), trans = scales::log10_trans()) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(lambda_l2 = "L2 Regularization"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
cat_smooth <- function(range = c(10.0, 100.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(max_cat_threshold = "Categorical Smoothing"),
    finalize = NULL
  )
}


#' @rdname param_lgbm
#' @export
cat_l2 <- function(range = c(-3, 2), trans = scales::log10_trans()) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(cat_l2 = "L2 Regularization in Categorical Split"),
    finalize = NULL
  )
}

# nocov end
