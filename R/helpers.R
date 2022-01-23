#' Strip attribute data from CV objects to shrink their size
#'
#' @description Remove unnecessary attribute data from iteration results objects
#' created by \code{\link[tune]{tune_grid}} and \code{\link[tune]{tune_bayes}}.
#' This makes the parameter object significantly smaller when saved to disk.
#'
#' @param x Iteration object created by the tune package.
#'
#' @return Iteration object from \code{x} but with CV split and attributes
#'   removed.
#'
#' @export
axe_tune_data <- function(x) {
  stripped <- dplyr::select(x, -dplyr::any_of("splits"))
  attrs <- purrr::list_modify(attributes(x), "names" = names(stripped))
  attributes(stripped) <- attrs

  return(stripped)
}


#' Strip environment data from recipe objects to shrink their size
#'
#' @description Recipe objects used by Tidymodels sometimes become bloated due
#' to quosures and saved factor levels. This function will strip all data from
#' a recipe object except the data necessary to use \code{\link[recipes]{bake}}.
#'
#' This saves an enormous amount of disk space for certain complicated recipes
#' that need to be saved and re-used.
#'
#' @param x A prepped recipe object created by the recipe package.
#'
#' @return Recipe object from \code{x} but with environment and factor level
#'   data removed.
#'
#' @export
axe_recipe <- function(x) {
  stopifnot(class(x) == "recipe")

  axed <- rapply(x, butcher::butcher, how = "replace")
  axed <- purrr::list_modify(axed, orig_lvls = NULL)
  class(axed) <- "recipe"

  return(axed)
}


#' Predict values using a trained model and recipe
#'
#' @description Simple helper function to return predictions from a new data set
#' given a parsnip specification and recipe. Will exponentiate predictions by
#' default.
#'
#' @param spec A parsnip model specification object. Must be trained.
#' @param recipe A prepped recipe object. Must be trained.
#' @param data New data to get predictions from. Will be pre-processed by the
#'   specified \code{recipe}.
#'
#' @return A vector of predictions from the model given the data and recipe
#'   specified.
#'
#' @export
lgbm_predict <- function(spec, recipe, data) {
  pred <- parsnip::predict.model_fit(
    object = spec,
    new_data = recipes::bake(recipe, data, recipes::all_predictors())
  )$.pred

  return(pred)
}


#' Finalize workflow objects for LightGBM models
#'
#' @description LightGBM is currently not natively supported by Tidymodels.
#' As such, functions such as \code{\link[tune]{finalize_workflow}}, which is
#' usually responsible for passing final hyperparameters from cross-validation
#' into a workflow object, do not function correctly.
#'
#' This function is a hacky workaround which inserts parameters directly into
#' the workflow object by looking for their names (workflows are just nested
#' lists), then inserting the matching values passed in \code{params}.
#'
#' This function will likely be deprecated by in the near future.
#'
#' @param wflow A workflow object created by \code{\link[workflows]{workflow}}.
#'
#' @param params A dataframe or named list of parameters. Usually created by
#'   \code{\link[tune]{select_best}}.
#'
#' @return A finalized workflow object with updated parameters.
#'
#' @export
lgbm_update_params <- function(wflow, params) {
  wflow$fit$actions$model$spec <- purrr::list_modify(
    wflow$fit$actions$model$spec,
    as.list(dplyr::select(params, -dplyr::any_of(".config")))
  )
  return(wflow)
}


#' Save a LightGBM model to disk
#'
#' Save a parsnip model fit object with a LightGBM fit to disk. This is a
#' a workaround to split the LightGBM model and parsnip specification into
#' separate files and then zip them together. This is necessary because LightGBM
#' models really do not like being saved with \code{saveRDS()}.
#'
#' @param model A parsnip specification containing a fit LightGBM model.
#' @param zipfile A path to save zip file to.
#'
#' @export
lgbm_save <- function(model, zipfile) {
  file_lgbm <- file.path(tempdir(), "lgbm.model")
  lightgbm::lgb.save(model$fit, file_lgbm)
  model$fit <- NULL

  file_meta <- file.path(tempdir(), "meta.model")
  saveRDS(model, file_meta)

  zip::zipr(zipfile, files = c(file_meta, file_lgbm))
}


#' Load a LightGBM model from disk
#'
#' This is the paired function to \code{model_lgbm_save()}. It loads a zip file
#' containing a parsnip specification and LightGBM model, then recombines them
#' into a single object.
#'
#' @param zipfile A path to the zip file containing the saved model objects.
#'
#' @return A parsnip model fit with LightGBM model.
#'
#' @export
lgbm_load <- function(zipfile) {
  ex_dir <- tempdir()
  zip::unzip(zipfile, exdir = ex_dir)

  model <- readRDS(file.path(ex_dir, "meta.model"))
  model$fit <- lightgbm::lgb.load(file.path(ex_dir, "lgbm.model"))

  return(model)
}