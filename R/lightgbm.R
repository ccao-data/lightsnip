# nocov start
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

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "stop_iter",
    original = "early_stop",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )
}


# nocov end

#' Boosted trees via LightGBM
#'
#' @description Wrapper for \code{\link[lightgbm]{lgb.train}} tree-based models
#' with some expanded/advanced options.
#'
#' @param x A matrix of predictors.
#' @param y A numeric vector of outcome data.
#' @param num_iterations Integer value for the number of iterations (trees)
#'   to grow.
#' @param max_depth Integer value for the maximum leaf distance from the root
#'   node.
#' @param num_leaves Integer value for the maximum possible number of leaves
#'   in one tree.
#' @param link_max_depth Logical, default FALSE. When TRUE, and when
#'   \code{max_depth} is unconstrained \code{-1}, then \code{max_depth} will
#'   be set to \code{floor(log2(num_leaves)) + link_max_depth_add}.
#' @param add_to_linked_depth Integer value to add to \code{max_depth} when it
#'   is linked to \code{num_leaves}.
#' @param categorical_feature A character vector of feature names or an
#'   integer vector with the indices of the features.
#' @param validation A positive number. If on \code{[0, 1)}
#'   the value, \code{validation} is a random proportion of data
#'   in \code{x} and \code{y} that are used for performance assessment and
#'   potential early stopping. If 1 or greater, it is the _number_
#'   of training set samples use for these purposes.
#' @param early_stop An integer or \code{NULL}. If not \code{NULL}, it is the
#'   number of training iterations without improvement before stopping.
#'   If \code{validation} is used, performance is base on the validation set;
#'   otherwise the training set is used.
#' @param max_bin Max number of bins that feature values will be bucketed in.
#' @param feature_pre_filter Tell LightGBM to ignore the features that are
#'   unsplittable based on \code{min_data_in_leaf}.
#' @param verbose Integer. < 0: Fatal, = 0: Error (Warning), = 1: Info,
#'   > 1: Debug.
#' @param ... Engine arguments, hyperparameters, etc. that are passed on to
#'   \code{\link[lightgbm]{lgb.train}}.
#'
#' @return A fitted \code{lgb.Booster} object.
#' @keywords internal
#' @export
train_lightgbm <- function(x,
                           y,
                           num_iterations = 10,
                           max_depth = 17,
                           num_leaves = 31,
                           link_max_depth = FALSE,
                           add_to_linked_depth = 2L,
                           categorical_feature = NULL,
                           validation = 0,
                           early_stop = NULL,
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


  ##### Early Stopping #####
  if (!is.numeric(validation) || validation < 0 || validation >= 1) {
    rlang::abort("`validation` should be on [0, 1).")
  }
  if (!is.null(early_stop)) {
    if (early_stop <= 1) {
      rlang::abort(
        paste0("`early_stop` should be on [2, ",  num_iterations, ").")
      )
    } else if (early_stop >= num_iterations) {
      early_stop <- num_iterations - 1
      rlang::warn(
        paste0("`early_stop` was reduced to ", early_stop, ".")
      )
    }
  }


  ##### Arguments #####
  # If linked, set max_depth slightly higher than depth-first
  if (link_max_depth && max_depth == -1L) {
    max_depth <- floor(log2(num_leaves)) + add_to_linked_depth
  }
  if (is.null(num_leaves) && max_depth > 0) {
    num_leaves <- max(2^max_depth - 1, 2)
  }

  arg_list <- list(
    max_depth = max_depth,
    num_leaves = num_leaves
  )

  others <- others[!(names(others) %in% c("data", names(arg_list)))]
  arg_list <- purrr::compact(c(arg_list, others))


  ##### Setup Data #####
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

  n <- nrow(x)
  if (validation > 0) {
    trn_index <- sample(1:n, size = floor(n * validation) + 1)
    valids <- list(validation = lightgbm::lgb.Dataset(
        data = as.matrix(x[-trn_index, ]),
        label = y[-trn_index],
        categorical_feature = categorical_feature,
        params = data_arg_list
      ))
    d <- lightgbm::lgb.Dataset(
      data = as.matrix(x[trn_index, ]),
      label = y[trn_index],
      categorical_feature = categorical_feature,
      params = data_arg_list
    )

  } else {
    d <- lightgbm::lgb.Dataset(
      data = as.matrix(x),
      label = y,
      categorical_feature = categorical_feature,
      params = data_arg_list
    )
    valids <- list(training = d)
  }


  ##### Train #####
  main_args <- list(
    params = arg_list,
    data = quote(d),
    nrounds = num_iterations,
    valids = quote(valids),
    early_stopping_rounds = early_stop,
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
