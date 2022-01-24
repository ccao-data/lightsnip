# Lightsnip package <a href='https://gitlab.com/ccao-data-science---modeling/packages/lightsnip'><img src='man/figures/logo.png' align="right" height="139" /></a>

Lightsnip is a hard fork of [curso-r/treesnip](https://github.com/curso-r/treesnip). It adds LightGBM bindings for parsnip and enables more advanced LightGBM features, such as early stopping. It is not intended for general use, only as a dependency for CCAO regression models.

For detailed documentation on included functions, [**visit the
full reference
list**](https://ccao-data-science---modeling.gitlab.io/packages/lightsnip/reference/).

## Installation

You can install the released version of `lightsnip` directly from GitLab by
running the following R command after installing
[remotes](https://github.com/r-lib/remotes):

```r
remotes::install_gitlab("ccao-data-science---modeling/packages/lightsnip")
```

Occasionally, when using brand-new or source versions of packages,
installation [on Windows will fail with the following
error](https://github.com/rstudio/renv/issues/162):

```
DLL 'package_name' not found: maybe not installed for this architecture?
```

If this happens, try using the following installation command:

```r
remotes::install_gitlab(
  repo = "ccao-data-science---modeling/packages/lightsnip",
  INSTALL_opts = "--no-multiarch"
)
```

## Differences compared to [treesnip](https://github.com/curso-r/treesnip)

- Removed support for `tree` and `catboost` (LightGBM only)
- Removed classification support for LightGBM (regression only)
- Removed treesnip caps on `max_depth`, other parameters
- Removed vignettes and samples
- Added LightGBM-specific hyperparameter functions
- Added model save/load helpers
- Added recipe/fit cleaning helpers
- Force user to specify categorical columns by name, does _not_ implicitly convert factors to categoricals
- Added early stopping from xgboost
- Added more unit tests
- Fixed a number of bugs
