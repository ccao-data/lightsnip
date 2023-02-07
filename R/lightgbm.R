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
#' @param weight A numeric vector of sample weights. Should be the same length
#'   as the number of rows of \code{x}.
#' @param validation A positive number on \code{[0, 1)}. \code{validation} is
#'   the proportion of data in \code{x} and \code{y} that is used for
#'   performance assessment and early stopping.
#' @param sample_type The sampling method for the validation set. Can be either
#'   "random" (a completely random sample) or "recent" (the last X% of rows,
#'   where X is the proportion specified by \code{validation}).
#' @param early_stop An integer or \code{NULL}. If an integer, it is the
#'   number of iterations without improvement before stopping.
#'   Must be set when \code{validation} is > 0.
#' @param max_bin Max number of bins that feature values will be bucketed in.
#' @param feature_pre_filter Tell LightGBM to ignore the features that are
#'   unsplittable based on \code{min_data_in_leaf}.
#' @param free_raw_data LightGBM constructs its data format, called a "Dataset",
#'   from tabular data. By default, that Dataset object on the R side does not
#'   keep a copy of the raw data. This reduces LightGBM's memory consumption,
#'   but it means that the Dataset object cannot be changed after it has been
#'   constructed. If you'd prefer to be able to change the Dataset object after
#'   construction, set \code{free_raw_data = FALSE}. Useful for debugging.
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
                           weight = NULL,
                           validation = 0,
                           sample_type = "random",
                           early_stop = NULL,
                           max_bin = NULL,
                           feature_pre_filter = FALSE,
                           free_raw_data = TRUE,
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
        paste0("`early_stop` should be on [2, ", num_iterations, ").")
      )
    } else if (early_stop >= num_iterations) {
      early_stop <- num_iterations - 1
      rlang::warn(
        paste0(
          "`early_stop` is greater than `num_iterations`.",
          "`early_stop` reduced to ", early_stop, "."
        )
      )
    }
  }
  if (is.null(early_stop) && validation > 0) {
    rlang::abort("If `validation` is > 0, then `early_stop` must also be set.")
  }
  if (!sample_type %in% c("random", "recent")) {
    rlang::abort("`sample_type` must be one of: 'random', 'recent'.")
  }


  ##### Arguments #####
  # If linked, set max_depth slightly higher than depth-first
  if (link_max_depth) {
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

  n <- nrow(x)
  if (validation > 0) {
    m <- min(floor(n * (1 - validation)), n - 1)
    if (sample_type == "random") {
      m <- min(floor(n * (1 - validation)), n - 1)
      trn_index <- sample(1:n, size = max(m, 2))
      val_index <- setdiff(1:n, trn_index)
    } else if (sample_type == "recent") {
      m <- min(n - floor(n * validation), n - 1)
      trn_index <- seq(1, max(m, 2))
      val_index <- setdiff(1:n, trn_index)
    }
    valids <- list(validation = lightgbm::lgb.Dataset(
      data = as.matrix(x[val_index, , drop = FALSE]),
      label = y[val_index],
      categorical_feature = categorical_feature,
      params = data_arg_list,
      weight = weight[val_index],
      free_raw_data = free_raw_data
    ))
  } else {
    trn_index <- 1:n
  }

  d <- lightgbm::lgb.Dataset(
    data = as.matrix(x[trn_index, , drop = FALSE]),
    label = y[trn_index],
    categorical_feature = categorical_feature,
    params = data_arg_list,
    weight = weight[trn_index],
    free_raw_data = free_raw_data
  )


  ##### Train #####
  main_args <- list(
    params = arg_list,
    data = quote(d),
    nrounds = num_iterations,
    verbose = verbose
  )

  if (!is.null(early_stop) && validation > 0) {
    main_args$valids <- quote(valids)
    main_args$early_stopping_rounds <- early_stop
  }

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
#'   of the \code{lgb.Booster} object.
#'
#' @export
pred_lgb_reg_num <- function(object, new_data, ...) {
  stats::predict(
    object$fit,
    as.matrix(new_data),
    reshape = TRUE,
    params = list(predict_disable_shape_check = TRUE),
    ...
  )
}

# nolint start
#' Model predictions across many sub-models
#'
#' For some models, predictions can be made on sub-models in the model object.
#'
#' @param object A model_fit object.
#' @param ... Optional arguments to pass to
#'   \code{predict.model_fit(type = "raw")}, such as type.
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or NULL. Possible values are
#'   "numeric", "class", "prob", "conf_int", "pred_int", "quantile", or "raw".
#'   When NULL, \code{predict()} will choose an appropriate value based on the
#'   model's mode.
#' @param trees An integer vector for the number of trees in the ensemble.
#'
#' @export
#' @importFrom purrr map_df
#' @importFrom parsnip multi_predict
multi_predict._lgb.Booster <- function(object,
                                       new_data,
                                       type = NULL,
                                       trees = NULL,
                                       ...) {
  if (any(names(rlang::enquos(...)) == "newdata")) {
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
  }

  # nolint end
  trees <- sort(trees)
  res <- purrr::map_df(
    trees,
    lightgbm_by_tree,
    object = object,
    new_data = new_data,
    type = type
  )
  res <- dplyr::arrange(res, .row, trees)
  res <- split(res[, -1], res$.row)
  names(res) <- NULL

  tibble::tibble(.pred = res)
}


lightgbm_by_tree <- function(tree, object, new_data, type = NULL) {
  pred <- pred_lgb_reg_num(object, new_data, num_iteration = tree)
  pred <- tibble::tibble(.pred = pred)
  nms <- names(pred)
  pred[["trees"]] <- tree
  pred[[".row"]] <- seq_len(nrow(new_data))
  pred[, c(".row", "trees", nms)]
}
