context("test axe_tune_data()")

##### TEST axe_tune_data() #####

axe_test_data <- dplyr::tibble(
  PIN = rep("12345", 4),
  EXT_WALL = c("1", "2", "0", NA),
  splits = c("1", "3", "4", "5")
)

# Test for expected outputs
test_that("output is as expected", {
  expect_equivalent(axe_tune_data(axe_test_data), axe_test_data[, 1:2])
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(axe_tune_data("cat"))
  expect_condition(axe_tune_data(8))
})


context("test axe_recipe()")

##### TEST axe_recipe() #####

iris_recp <- recipes::recipe(
  Sepal.Length ~ Sepal.Width + Petal.Length + Species,
  data = iris
) %>%
  recipes::step_string2factor(Species)

iris_recp_prep <- recipes::prep(iris_recp)

test_that("orig_lvls are removed", {
  expect_no_match(names(axe_recipe(iris_recp_prep)), "orig_lvls")
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(axe_recipe("cat"))
  expect_condition(axe_recipe(8))
})


context("test lgbm_save() and lgbm_load()")

##### TEST lgbm_save() and lgbm_load() #####

library(modeldata)

# Prep lightgbm model
model <- parsnip::boost_tree(mtry = 1, trees = 50, tree_depth = 15, min_n = 1)
model <- parsnip::set_engine(
  model, "lightgbm",
  verbose = -1L, save_tree_error = TRUE
)
model <- parsnip::set_mode(model, "regression")

# Prep input data with recipe
data(attrition)
amnt <- c("nothin", "meh", "some", "copious")
attrition_recp <-
  recipes::recipe(
    HourlyRate ~ StockOptionLevel + TotalWorkingYears,
    data = attrition
  ) %>%
  recipes::step_num2factor(
    StockOptionLevel,
    transform = function(x) x + 1,
    levels = amnt
  ) %>%
  recipes::step_integer(recipes::all_nominal(), zero_based = TRUE)

# Train model
attrition_baked <- recipes::bake(
  recipes::prep(attrition_recp),
  attrition
)
adj <- parsnip::fit(
  model,
  HourlyRate ~ StockOptionLevel + TotalWorkingYears,
  data = attrition_baked
)

test_that("prediction outputs as expected", {
  expect_type(
    predict(
      adj,
      recipes::bake(recipes::prep(attrition_recp), attrition[1:100, ])
    )$.pred,
    "double"
  )
  expect_length(
    predict(
      adj,
      recipes::bake(recipes::prep(attrition_recp), attrition[1:100, ])
    )$.pred,
    100
  )
})

lgbm_workflow <- workflows::workflow(attrition_recp, model)
lgbm_workflow_fit <- lgbm_workflow %>%
  parsnip::fit(attrition[1:100, ])

test_that("model can be saved and loaded", {
  file <- tempfile(fileext = ".zip")
  lgbm_workflow_fit %>%
    workflows::extract_fit_parsnip() %>%
    lgbm_save(file)

  reloaded <- lgbm_load(file)

  # Make sure record_evals got preserved during saving/loading
  expect_equal(length(reloaded$fit$record_evals$tree_errors$l2$eval), 50)

  out <- predict(
    reloaded,
    recipes::bake(
      recipes::prep(attrition_recp),
      attrition[50:100, ]
    )
  )$.pred

  expect_type(out, "double")
  expect_length(out, 51)
})
