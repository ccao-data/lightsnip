# Create custom dials:: lamdda_l1 hyperparameter to use for tuning
# See: https://lightgbm.readthedocs.io/en/latest/Parameters.html#lambda_l1
lambda_l1 <- function(range = c(0.0, 100.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(lambda_l1 = "L1 Regularization"),
    finalize = NULL
  )
}

# Create custom dials:: lamdda_l2 hyperparameter to use for tuning
# See: https://lightgbm.readthedocs.io/en/latest/Parameters.html#lambda_l2
lambda_l2 <- function(range = c(0.0, 100.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(lambda_l2 = "L2 Regularization"),
    finalize = NULL
  )
}


# Create custom dials:: cat_smooth hyperparameter to use for tuning
# See: https://lightgbm.readthedocs.io/en/latest/Parameters.html#cat_smooth
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
