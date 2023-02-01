context("lintr coverage")

test_that("no lintr errors", {
  lintr::expect_lint_free(
    linters = lintr::linters_with_defaults(
      cyclocomp_linter = lintr::cyclocomp_linter(30),
      object_name_linter = NULL,
      object_usage_linter = NULL
    ),
    path = "../.."
  )
})
