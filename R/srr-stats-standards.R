#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose FALSE
#'
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @noRd
#' @srrstatsNA {G2.4a} *explicit conversion to `integer` via `as.integer()`*
#' All expected outcomes are data.frames
#' @srrstatsNA {G2.4b} *explicit conversion to continuous via `as.numeric()`*
#' All expected outcomes are data.frames
#' @srrstatsNA {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' All expected outcomes are data.frames
#' @srrstatsNA {G2.4d} *explicit conversion to factor via `as.factor()`*
#' All expected outcomes are data.frames
#' @srrstatsNA {G2.4e} *explicit conversion from factor via `as...()` functions*
#' All expected outcomes are data.frames
#' @srrstatsNA {G2.14c} *replace missing data with appropriately imputed values*
#' Missing data will throw an error as per G2.14a or a warning as per G2.14b
#' @srrstatsNA {G3.1} *Statistical software which relies on covariance calculations should enable users to choose between different algorithms for calculating covariances, and should not rely solely on covariances from the `stats::cov` function.*
#' No reliance on covariance calculations
#' @srrstatsNA {G3.1a} *The ability to use arbitrarily specified covariance methods should be documented (typically in examples or vignettes).*
#' No reliance on covariance calculations
#' @srrstatsNA {G2.2} *Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.*
#' no univariate inputs expected
#' @srrstatsNA {G2.3} *For univariate character input:*
#' no univariate inputs expected
#' @srrstatsNA {G2.3a} *Use `match.arg()` or equivalent where applicable to only permit expected values.*
#' no univariate inputs expected
#' @srrstatsNA {G2.3b} *Either: use `tolower()` or equivalent to ensure input of character parameters is not case dependent; or explicitly document that parameters are strictly case-sensitive.*
#' no univariate inputs expected
#' @srrstatsNA {G4.0} *Statistical Software which enables outputs to be written to local files should parse parameters specifying file names to ensure appropriate file suffices are automatically generated where not provided.*
#' The package cannot write to local files
#' @srrstatsNA {G5.4a} *For new methods, it can be difficult to separate out correctness of the method from the correctness of the implementation, as there may not be reference for comparison. In this case, testing may be implemented against simple, trivial cases or against multiple implementations such as an initial R implementation compared with results from a C/C++ implementation.*
#' @srrstatsNA {G5.4b} *For new implementations of existing methods, correctness tests should include tests against previous implementations. Such testing may explicitly call those implementations in testing, preferably from fixed-versions of other software, or use stored outputs from those where that is not possible.*
#' function runs lm, feols, or felm under the hood, which have been thoroughly tested
#' @srrstatsNA {G5.4c} *Where applicable, stored values may be drawn from published paper outputs when applicable and where code from original implementations is not available*
#' No extended test structure expected
#' @srrstatsNA {G5.10} *Extended tests should included and run under a common framework with other tests but be switched on by flags such as as a `<MYPKG>_EXTENDED_TESTS="true"` environment variable.* - The extended tests can be then run automatically by GitHub Actions for example by adding the following to the `env` section of the workflow:
#' No extended test structure expected
#' @srrstatsNA {G5.11} *Where extended tests require large data sets or other assets, these should be provided for downloading and fetched as part of the testing workflow.*
#' No extended test structure expected
#' @srrstatsNA {G5.11a} *When any downloads of additional data necessary for extended tests fail, the tests themselves should not fail, rather be skipped and implicitly succeed with an appropriate diagnostic message.*
#' No extended test structure expected
#' @srrstatsNA {G5.12} *Any conditions necessary to run extended tests such as platform requirements, memory, expected runtime, and artefacts produced that may need manual inspection, should be described in developer documentation such as a `CONTRIBUTING.md` or `tests/README.md` file.*
#' No extended test structure expected
#' @srrstatsNA {EA5.4} *All visualisations should ensure values are rounded sensibly (for example, via `pretty()` function).*
#' No visualisations as outputs
#' @srrstatsNA {EA5.5} *All visualisations should include units on all axes where such are specified or otherwise obtainable from input data or other routines.*
#' No visualisations as outputs
#' @srrstatsNA {EA5.6} *Any packages which internally bundle libraries used for dynamic visualization and which are also bundled in other, pre-existing R packages, should explain the necessity and advantage of re-bundling that library.*
#' No visualisations as outputs
#' @srrstatsNA {EA2.0} *EDA Software which accepts standard tabular data and implements or relies upon extensive table filter and join operations should utilise an **index column** system*
#' No operations that require table indexes in package
#' @srrstatsNA {EA2.1} *All values in an index column must be unique, and this uniqueness should be affirmed as a pre-processing step for all input data.*
#' No operations that require table indexes in package
#' @srrstatsNA {EA2.2} *Index columns should be explicitly identified, either:*
#' No operations that require table indexes in package
#' @srrstatsNA {EA2.2a} *by using an appropriate class system, or*
#' No operations that require table indexes in package
#' @srrstatsNA {EA2.2b} *through setting an `attribute` on a table, `x`, of `attr(x, "index") <- <index_col_name>`.*
#' No operations that require table indexes in package
#' @srrstatsNA {EA2.3} *Table join operations should not be based on any assumed variable or column names*
#' No operations that require table indexes in package
#' @srrstatsNA {EA2.4} *Use and demand an explicit class system for such input (for example, via the [`DM` package](https://github.com/krlmlr/dm)).*
#' No operations that require table indexes in package
#' @srrstatsNA {EA2.5} *Ensure all individual tables follow the above standards for Index Columns*
#' No operations that require table indexes in package
#' @srrstatsNA {EA6.1} *The properties of graphical output from EDA software should be explicitly tested, for example via the [`vdiffr` package](https://github.com/r-lib/vdiffr) or equivalent.*
#' No graphical output
#' @srrstatsNA {EA6.0e} *Values of single-valued objects; for `numeric` values either using `testthat::expect_equal()` or equivalent with a defined value for the `tolerance` parameter, or using `round(..., digits = x)` with some defined value of `x` prior to testing equality.*
#' No single-valued objects returned
#' @srrstatsNA {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' These aspects are covered by lm, felm, and feols, which run under the hood of partialling_out
#' @srrstatsNA {G2.5} *Where inputs are expected to be of `factor` type, secondary documentation should explicitly state whether these should be `ordered` or not, and those inputs should provide appropriate error or other routines to ensure inputs follow these expectations.*
NULL

