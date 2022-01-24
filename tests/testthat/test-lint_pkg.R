context("lintr coverage")

test_that("no lintr errors", {
  lintr::expect_lint_free(
    linters = lintr::with_defaults(
      cyclocomp_linter = lintr::cyclocomp_linter(30)
    )
  )
})
