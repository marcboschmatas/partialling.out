#' @keywords internal
"_PACKAGE"

## usethis namespace: start

# nolint start
#' @srrstats {G1.0} *This package is an implementation of the Frisch-Waugh-Lovell theorem as explained in [(Lowell, 2008)](https://www.tandfonline.com/doi/abs/10.3200/JECE.39.1.88-91)*
#' @srrstats {G1.1} *The implemented algorithm is* - *An improvement on other implementations of similar algorithms in **R***.
#' @srrstats {G1.2} *In a stable state of development, with some degree of active subsequent development as envisioned by the primary authors.*
#' @srrstats {G1.3} *All terms have been defined according to the relevant literature*
#' @srrstats {G1.4} *[`roxygen2`](https://roxygen2.r-lib.org/) has been used*
#' @srrstats {G1.4a} *All internal functions have been documented*
#' @srrstats {G1.5} *No associated publications have been made yet, but vignettes are fully reproducible.*
#' @srrstats {G1.6} *Comparisons with `fwlplot` have been made in the vignettes.*
#' @srrstats {EA1.0} *The package is intended mostly for econometricians and other quantitative social scientists in order to allow for quick model visualisation and assessment*
#' @srrstats {EA1.1} *The package is geared to analyse linear and fixed effects models (As produced by `stats::lm()`, `fixest::feols()`, and `lfe::felm()`*
#' @srrstats {EA1.2} *The objective of this package is to simplify complex linear and fixed effects models and to provide simpler visualisations of the relation between the variable of interest and the main explanatory variable*
#' @srrstats {EA1.3} *Functions accept statistical models (`lm`, `feols`, and `felm`) and data.frames or its extensions (tibble, data.table...)*

#' @import rlang
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

# nolint end
