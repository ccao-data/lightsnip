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
  attributes(stripped)$rset_info$att$splits <- NULL
  attributes(stripped)$rset_info$att$id <- NULL

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
  axed <- utils::modifyList(axed, list(orig_lvls = NULL))
  class(axed) <- "recipe"

  return(axed)
}


#' Save a LightGBM model to disk
#'
#' Save a parsnip model fit object with a LightGBM fit to disk. This is a
#' a workaround to split the LightGBM model and parsnip specification into
#' separate files and then zip them together. This is necessary because LightGBM
#' models really do not like being saved with \code{\link{saveRDS}}.
#'
#' @param model A parsnip specification containing a fit LightGBM model.
#' @param zipfile A path to save zip file to.
#'
#' @export
lgbm_save <- function(model, zipfile) {
  file_lgbm <- file.path(tempdir(), "lgbm.model")
  lightgbm::lgb.save(model$fit, file_lgbm)

  # The record_evals attribute of the model object, when present, gets stripped
  # out by lgb.save(), so save it to a separate file
  file_record_evals <- file.path(tempdir(), "record_evals.model")
  saveRDS(model$fit$record_evals, file_record_evals)

  model$fit <- NULL
  file_meta <- file.path(tempdir(), "meta.model")
  saveRDS(model, file_meta)

  zip::zipr(zipfile, files = c(file_meta, file_lgbm, file_record_evals))
}


#' Load a LightGBM model from disk
#'
#' This is the paired function to \code{\link{lgbm_save}}. It loads a zip
#' file containing a parsnip specification and LightGBM model, then recombines
#' them into a single object.
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

  # For backwards-compatibility, only load record_evals if they exist
  record_evals_path <- file.path(ex_dir, "record_evals.model")
  if (file.exists(record_evals_path)) {
    model$fit$record_evals <- readRDS(record_evals_path)
  }

  return(model)
}
