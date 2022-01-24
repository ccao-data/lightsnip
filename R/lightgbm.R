#' Wrapper to add \code{lightgbm} engine to the parsnip \code{boost_tree} model
#' specification. Gets called when the package loads
#'
#' @return NULL
#' @export
add_boost_tree_lightgbm <- function() {
  parsnip::set_model_engine("boost_tree", mode = "regression", eng = "lightgbm")
  parsnip::set_dependency("boost_tree", eng = "lightgbm", pkg = "lightgbm")
  parsnip::set_dependency("boost_tree", eng = "lightgbm", pkg = "lightsnip")

  parsnip::set_fit(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "lightsnip", fun = "train_lightgbm"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    mode = "regression",
    eng = "lightgbm",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "lightsnip", fun = "pred_lgb_reg_num"),
      args = list(
        object = quote(object),
        new_data = quote(new_data)
      )
    )
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "trees",
    original = "num_iterations",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = TRUE
  )
}


#' Boosted trees via LightGBM
#'
#' @description \code{\link{lightgbm_train}} is a wrapper for
#'  \code{\link[lightgbm]{lgb.train}} tree-based models.
#'
#' @param x A matrix of predictors.
#' @param y A numeric vector of outcome data.
#' @param num_iterations Integer value for the number of iterations (trees)
#'   to grow.
#' @param max_depth Integer value for the maximum leaf distance from the root
#'   node.
#' @param num_leaves Integer value for the maximum possible number of leaves
#'   in one tree.
#' @param learning_rate A numeric value between zero and one to control
#'   the learning rate.
#' @param feature_fraction Subsampling proportion of columns.
#' @param min_data_in_leaf A numeric value for the minimum sum of instances
#'   needed in a child to continue to split.
#' @param min_gain_to_split A number for the minimum loss reduction required
#'   to make a further partition on a leaf node of the tree.
#' @param link_max_depth Logical, default FALSE. When TRUE, and when
#'   \code{max_depth} is unconstrained \code{-1}, then \code{max_depth} will
#'   be set to \code{floor(log2(num_leaves)) + link_max_depth_add}.
#' @param add_to_linked_depth Integer value to add to \code{max_depth} when it
#'   is linked to \code{num_leaves}.
#' @param categorical_feature A character vector of feature names or an
#'   integer vector with the indices of the features.
#' @param max_bin Max number of bins that feature values will be bucketed in.
#' @param feature_pre_filter Tell LightGBM to ignore the features that are
#'   unsplittable based on \code{min_data_in_leaf}.
#' @param verbose Integer. < 0: Fatal, = 0: Error (Warning), = 1: Info,
#'   > 1: Debug.
#' @param ... Engine arguments, hyperparameters, etc. that are passed on to
#'   \code{\link[lightgbm]{lgb.train}}.
#'
#' @return A fitted \code{lgb.Booster} object.
#' @export
train_lightgbm <- function(x,
                           y,
                           num_iterations = 10,
                           max_depth = 17,
                           num_leaves = 31,
                           learning_rate = 0.1,
                           feature_fraction = 1,
                           min_data_in_leaf = 20,
                           min_gain_to_split = 0,
                           link_max_depth = FALSE,
                           add_to_linked_depth = 2L,
                           categorical_feature = NULL,
                           max_bin = NULL,
                           feature_pre_filter = FALSE,
                           verbose = 0,
                           ...) {
  force(x)
  force(y)
  others <- list(...)

  # Set training objective (always regression)
  if (!any(names(others) %in% c("objective"))) {
    others$num_class <- 1
    others$objective <- "regression"
  }

  # Parallelism should be explicitly specified by the user
  nthreads_args <- c(
    "num_threads", "num_thread",
    "nthread", "nthreads", "n_jobs"
  )
  if (all(sapply(others[nthreads_args], is.null))) others$num_threads <- 1L

  # If linked, set max_depth slightly higher than depth-first
  if (link_max_depth & max_depth == -1L) {
    max_depth <- floor(log2(num_leaves)) + add_to_linked_depth
  }

  if (is.null(num_leaves)) {
    num_leaves <- max(2 ^ max_depth - 1, 2)
  }

  arg_list <- list(
    num_iterations = num_iterations,
    max_depth = max_depth,
    num_leaves = num_leaves,
    learning_rate = learning_rate,
    feature_fraction = feature_fraction,
    min_data_in_leaf = min_data_in_leaf,
    min_gain_to_split = min_gain_to_split
  )

  others <- others[!(names(others) %in% c("data", names(arg_list)))]
  arg_list <- purrr::compact(c(arg_list, others))


  ##### Train #####
  data_arg_list <- purrr::compact(list(
    feature_pre_filter = feature_pre_filter,
    max_bin = max_bin
  ))

  d <- lightgbm::lgb.Dataset(
    data = as.matrix(x),
    label = y,
    categorical_feature = categorical_feature,
    params = data_arg_list
  )

  main_args <- list(
    data = quote(d),
    params = arg_list,
    verbose = verbose
  )

  call <- parsnip::make_call(fun = "lgb.train", ns = "lightgbm", main_args)
  rlang::eval_tidy(call, env = rlang::current_env())
}


#' predict_lightgbm_regression_numeric
#'
#' Not intended for direct use.
#'
#' @param object A fitted object.
#' @param new_data Data frame in which to look for variables with
#'   which to predict.
#' @param ... Additional named arguments passed to the \code{predict()} method
#'   of the lgb.Booster object passed to object.
#'
#' @export
pred_lgb_reg_num <- function(object, new_data, ...) {
  stats::predict(object$fit, as.matrix(new_data),
    reshape = TRUE,
    params = list(predict_disable_shape_check = TRUE), ...
  )
}
