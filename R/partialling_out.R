
# nolint start
#' Creates a data.frame of the residualised main explanatory variable and,
#' if wanted, variable of interest of a linear or fixed effects model
#'
#' The function regresses the main (i.e. first in the model) explanatory
#' variable and the variable of interest (if parameter `both` is set to `TRUE`)
#' against all other control variables and fixed effects and returns the
#' residuals in a data.frame
#'
#'
#' Will accept lm, felm (lfe package), and feols (fixest package) objects
#' @title partialling_out: partialling out variable of interest and main
#' @param model object for which we want to residualise variables
#' @param data data.frame used in the original model. Using different data will
#' return unexpected results or an error.
#' @param weights a numeric vector for weighting the partial models
#' @param both if `TRUE` will residualise both the variable of interest and the
#' first explanatory variable in the model. If `FALSE`, only the latter.
#' Set to `TRUE` by default
#' @param na.rm if `TRUE` will remove observations with NA before any models are
#' run. If `FALSE`, the underlying `lm`, `feols`, or `felm` will remove NA
#' values but errors may arise if weights are used.
#' @param ... Any other lm, feols, or felm parameters that will be passed to the
#' partial regressions
#' @returns a data.frame with the (residualised) variable of interest and
#' residualised main explanatory variable
#' @examples
#' \donttest{library(palmerpenguins)
#' library(fixest)
#' model <- feols(bill_length_mm ~ bill_depth_mm | species + island,
#'                data = penguins)
#' partial_df <- partialling_out(model, penguins, both = TRUE)
#' }
#' @srrstats {G2.0} *Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so.*
#' @srrstats {G2.0a} *Provide explicit secondary documentation of any expectations on lengths of inputs*
#' @srrstats {G2.1} *Implement assertions on types of inputs (see the initial point on nomenclature above).*
#' @srrstats {G2.1a} *Provide explicit secondary documentation of expectations on data types of all vector inputs.*
#' @srrstats {G2.14} *Where possible, all functions should provide options for users to specify how to handle missing (`NA`) data, with options minimally including:*
#' @srrstats {G2.13} *Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms.*
#' @srrstats {G2.14a} *error on missing data*
#' @srrstats {G2.14b} *ignore missing data with default warnings or messages issued*
#' @srrstats {G2.15} *Functions should never assume non-missingness, and should never pass data with potential missing values to any base routines with default `na.rm = FALSE`-type parameters (such as [`mean()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/mean.html), [`sd()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/sd.html) or [`cor()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html)).*
#' @srrstats {G2.16} *All functions should also provide options to handle undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially ignoring or removing such values.*
#' @srrstats {G3.0} *Statistical software should never compare floating point numbers for equality. All numeric equality comparisons should either ensure that they are made between integers, or use appropriate tolerances for approximate equality.*
#' @srrstats {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstats {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#' @srrstats {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstats {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstats {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*
#' @srrstats {G5.3} *For functions which are expected to return objects containing no missing (`NA`) or undefined (`NaN`, `Inf`) values, the absence of any such values in return objects should be explicitly tested.*
#' @srrstats {G5.4} **Correctness tests** *to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding frameworks such as [RStata](https://github.com/lbraglia/RStata)).*
#' @srrstats {G5.5} *Correctness tests should be run with a fixed random seed*
#' @srrstats {G5.6} **Parameter recovery tests** *to test that the implementation produce expected results given data with known properties. For instance, a linear regression algorithm should return expected coefficient values for a simulated data set generated from a linear model.*
#' @srrstats {G5.6a} *Parameter recovery tests should generally be expected to succeed within a defined tolerance rather than recovering exact values.*
#' @srrstats {G5.6b} *Parameter recovery tests should be run with multiple random seeds when either data simulation or the algorithm contains a random component. (When long-running, such tests may be part of an extended, rather than regular, test suite; see G5.10-4.12, below).*
#' @srrstats {G5.7} **Algorithm performance tests** *to test that implementation performs as expected as properties of data change. For instance, a test may show that parameters approach correct estimates within tolerance as data size increases, or that convergence times decrease for higher convergence thresholds.*
#' @srrstats {G5.8} **Edge condition tests** *to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme properties including but not limited to:*
#' @srrstats {G5.8a} *Zero-length data*
#' @srrstats {G5.8b} *Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data)*
#' @srrstats {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns*
#' @srrstats {G5.8d} *Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)*
#' @srrstats {G5.9} **Noise susceptibility tests** *Packages should test for expected stochastic behaviour, such as through the following conditions:*
#' @srrstats {G5.9a} *Adding trivial noise (for example, at the scale of `.Machine$double.eps`) to data does not meaningfully change results*
#' @srrstats {G5.9b} *Running under different random seeds or initial conditions does not meaningfully change results*
#' @srrstats {G2.10} *Software should ensure that extraction or filtering of single columns from tabular inputs should not presume any particular default behaviour, and should ensure all column-extraction operations behave consistently regardless of the class of tabular data used as input.*
#' @srrstats {EA1.0} *Identify one or more target audiences for whom the software is intended*
#' @srrstats {EA1.1} *Identify the kinds of data the software is capable of analysing (see *Kinds of Data* below).*
#' @srrstats {EA1.2} *Identify the kinds of questions the software is intended to help explore.*
#' @srrstats {EA1.3} *Identify the kinds of data each function is intended to accept as input*
#' @srrstats {EA2.6} *Routines should appropriately process vector data regardless of additional attributes*
#' @srrstats {EA3.0} *The algorithmic components of EDA Software should enable automated extraction and/or reporting of statistics as some sufficiently "meta" level (such as variable or model selection), for which previous or reference implementations require manual intervention.*
#' @srrstats {EA3.1} *EDA software should enable standardised comparison of inputs, processes, models, or outputs which previous or reference implementations otherwise only enable in some comparably unstandardised form.*
#' @srrstats {EA4.0} *EDA Software should ensure all return results have types which are consistent with input types.*
#' @srrstats {EA4.1} *EDA Software should implement parameters to enable explicit control of numeric precision*
#' @srrstats {EA4.2} *The primary routines of EDA Software should return objects for which default `print` and `plot` methods give sensible results. Default `summary` methods may also be implemented.*
#' @srrstats {EA5.0} *Graphical presentation in EDA software should be as accessible as possible or practicable. In particular, EDA software should consider accessibility in terms of:*
#' @srrstats {EA5.0a} *Typeface sizes, which should default to sizes which explicitly enhance accessibility*
#' @srrstats {EA5.0b} *Default colour schemes, which should be carefully constructed to ensure accessibility.*
#' @srrstats {EA5.1} *Any explicit specifications of typefaces which override default values provided through other packages (including the `graphics` package) should consider accessibility*
#' @srrstats {EA5.2} *Screen-based output should never rely on default print formatting of `numeric` types, rather should also use some version of `round(., digits)`, `formatC`, `sprintf`, or similar functions for numeric formatting according the parameter described in* **EA4.1**.
#' @srrstats {EA6.0} *Return values from all functions should be tested, including tests for the following characteristics:*
#' @srrstats {EA6.0a} *Classes and types of objects*
#' @srrstats {EA6.0b} *Dimensions of tabular objects*
#' @srrstats {EA6.0c} *Column names (or equivalent) of tabular objects*
#' @srrstats {EA6.0d} *Classes or types of all columns contained within `data.frame`-type tabular objects *
#' @srrstats {EA5.3} *Column-based summary statistics should always indicate the `storage.mode`, `class`, or equivalent defining attribute of each column.*

#' @export
# nolint end
partialling_out <- function(model, data, weights, both, na.rm, ...) {
  UseMethod("partialling_out")
}

#' @importFrom stats formula
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom stats terms
#' @export
partialling_out.lm <- function(model, data = NULL,
                               weights = NULL, both = TRUE,
                               na.rm = TRUE, ...) {
  if (is.null(data)) {
    stop("No data has been provided")
  } # check data was provided
  if (!("data.frame" %in% class(data))) {
    stop("data input should be a data.frame")
  } # check data is a df
  # get formula
  f <- formula(model)
  terms_obj <- terms(f)
  y <- as.character(attr(terms_obj, "variables"))[2]
  x <-  attr(terms_obj, "term.labels")[1]
  controls <- attr(terms_obj, "term.labels")[-1]
  if (length(controls) == 0) {
    stop("No control variables found in the model.")
  }

  # clean
  data <- data[, c(y, x, controls)]
  original_nrow <- nrow(data) # for check later
  # remove nas
  if (na.rm) {
    navec <- apply(data,
                   1,
                   function(x) any(is.na(x)))
    data <- data[!navec, ]
  }else {
    warning("na.rm is set to FALSE and results depend on lm() na_action")
  }
  if (!is.null(weights) && is.null(model$weights)) {
    warning("Original model is not weighted, consider if weights are necessary")
  }
  if (is.null(weights) && !is.null(model$weights)) {
    warning("Original model is weighted, consider if weights should be added")
  }
  if (!is.null(weights) && !is.numeric(weights)) {
    stop("Weights should be a numeric vector")
  }
  if (!is.null(weights) && length(weights) != original_nrow) {
    stop("Length of weights is not equal to number of observations")
  }
  if (!is.null(weights) && na.rm) {
    weights <- weights[!navec]
  }




  if (both) {
    # make formula for y on controls
    formulay <- as.formula(paste0(y, " ~ ", paste0(controls, collapse = " + ")))
    # make formula for 1st x on controls
    formulax <- as.formula(paste0(x, " ~ ", paste0(controls, collapse = " + ")))
    resy <- lm(formulay, data = data, weights = weights, ...)$residuals
    resx <- lm(formulax, data = data, weights = weights, ...)$residuals
    resdf <- data.frame("y" = resy,
                        "x" = resx)

    colnames(resdf) <- paste0("res_", c(y, x))
  }else {
    formulax <- as.formula(paste0(x, " ~ ", paste0(controls, collapse = " + ")))
    resx <- lm(formulax, data = data, weights = weights, ...)$residuals
    resdf <- data.frame("y" = data[[y]],
                        "x" = resx)
    colnames(resdf) <- c(y, paste0("res_", x))
  }
  if (!is.null(weights)) {
    resdf <- cbind(resdf, weights)
    colnames(resdf)[3] <- "weights"
  }

  return(resdf)
}

#' @importFrom stats formula
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom fixest feols
#' @export
partialling_out.fixest <- function(model, data = NULL,
                                   weights = NULL, both = TRUE,
                                   na.rm = TRUE, ...) {
  # get data df - filter missing obs
  if (is.null(data)) {
    stop("No data has been provided")
  }  # check that a data object is supplied
  if (!("data.frame" %in% class(data))) {
    stop("data input should be a data.frame")
  } # check data was provided
  f <- formula(model)
  terms_obj <- terms(f)
  y <- as.character(attr(terms_obj, "variables"))[2]
  rhs <-  attr(terms_obj, "term.labels")

  terms_filter <- trimws(unlist(strsplit(rhs, "\\+|\\|")))
  # make sure no "0" or similar terms are passed to filtering function
  terms_filter <- terms_filter[terms_filter %in% colnames(data)]
  data <- data[, c(y, terms_filter)]
  original_nrow <- nrow(data) # for check later
  # remove NA if needed
  if (na.rm) {
    navec <- apply(data, 1, function(x) any(is.na(x))
    )
    data <- data[!navec, ]
  }else {
    warning("na.rm is set to FALSE and results depend on feols() na_action")
  }
  # make formulas for y and x
  # get indep vars, fe & inst vars
  rhs_split <- unlist(strsplit(rhs, "\\|"))
  x <- trimws(rhs_split[1]) # indep variables
  main_expvar <- trimws(unlist(strsplit(x, "\\+")))[1]
  controls <- trimws(unlist(strsplit(x, "\\+")))[-1]
  fe <- trimws(rhs_split[2])
  instvar <- trimws(rhs_split[3])
  if (length(controls) == 0 && is.na(fe)) {
    stop("No fixed effects or control variables found in the model.")
  }
  # make the common right hand side

  rhs2 <- if (length(controls) == 0) {
    c("1", fe, instvar)
  } else {
    c(controls, fe, instvar)
  }
  rhs2 <- rhs2[!is.na(rhs2)]
  rhs2 <- paste0(rhs2, collapse = " | ")

  # create residuals
  if (!is.null(weights) && is.null(model$weights)) {
    warning("Original model is not weighted, consider if weights are necessary")
  }

  if (is.null(weights) && !is.null(model$weights)) {
    warning("Original model is weighted, consider if weights should be added")
  }
  if (!is.null(weights) && !is.numeric(weights)) {
    stop("Weights should be a numeric vector")
  }
  if (!is.null(weights) && length(weights) != original_nrow) {
    stop("Length of weights is not equal to number of observations")
  }
  if (!is.null(weights) && na.rm) {
    weights <- weights[!navec]
  }

  if (both) {
    yformula <- paste(y, rhs2, sep = " ~ ")
    xformula <- paste(main_expvar, rhs2, sep = " ~ ")
    yres <- fixest::feols(as.formula(yformula), data = data,
                          weights = weights, ...)$residuals
    xres <- fixest::feols(as.formula(xformula), data = data,
                          weights = weights, ...)$residuals

    resdf <- data.frame("y" = yres,
                        "x" = xres)
    colnames(resdf) <- paste0("res_", c(y, main_expvar))
  }else {

    xformula <- paste(main_expvar, rhs2, sep = " ~ ")

    xres <- fixest::feols(as.formula(xformula), data = data,
                          weights = weights, ...)$residuals

    resdf <- data.frame("y" = data[[y]],
                        "x" = xres)
    colnames(resdf) <- c(y, paste0("res_", main_expvar))
  }
  if (!is.null(weights)) {
    resdf <- cbind(resdf, weights)
    colnames(resdf)[3] <- "weights"
  }


  return(resdf)
}

#' @importFrom stats formula
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom lfe felm
#' @export
partialling_out.felm <- function(model, data = NULL,
                                 weights = NULL, both = TRUE,
                                 na.rm = TRUE, ...) {
  # get data df - filter missing obs
  if (is.null(data)) {
    stop("No data has been provided")
  } # check that a data object is supplied
  if (!("data.frame" %in% class(data))) {
    stop("data input should be a data.frame")
  } # check data was provided
  f <- formula(model)
  terms_obj <- terms(f)
  y <- as.character(attr(terms_obj, "variables"))[2]
  rhs <-  attr(terms_obj, "term.labels")

  terms_filter <- trimws(unlist(strsplit(rhs, "\\+|\\|")))
  # make sure no "0" or similar terms are passed to filtering function
  terms_filter <- terms_filter[terms_filter %in% colnames(data)]
  data <- data[, c(y, terms_filter)]
  original_nrow <- nrow(data) # later check
  # filter data if needed
  if (na.rm) {
    navec <- apply(data, 1, function(x) any(is.na(x)))

    data <- data[!navec, ]
  }else {
    warning("na.rm is set to FALSE and results depend on felm() na_action")
  }
  # make formulas for y and x
  # get indep vars, fe & inst vars
  rhs_split <- unlist(strsplit(rhs, "\\|"))
  x <- trimws(rhs_split[1]) # indep variables
  main_expvar <- trimws(unlist(strsplit(x, "\\+")))[1]
  controls <- trimws(unlist(strsplit(x, "\\+")))[-1]

  fe <- trimws(rhs_split[2])
  instvar <- trimws(rhs_split[3])
  cluster <- trimws(rhs_split[4])
  if (length(controls) == 0 && is.na(fe)) {
    stop("No fixed effects or control variables found in the model.")
  }
  # make the common right hand side

  rhs2 <- if (length(controls) == 0) {
    c("1", fe, instvar, cluster)
  } else {
    c(controls, fe, instvar, cluster)
  }
  rhs2 <- rhs2[!is.na(rhs2)]
  rhs2 <- paste0(rhs2, collapse = " | ")


  # create residuals
  if (!is.null(weights) && is.null(model$weights)) {
    warning("Original model is not weighted, consider if weights are necessary")
  }
  if (is.null(weights) && !is.null(model$weights)) {
    warning("Original model is weighted, consider if weights should be added")
  }

  if (!is.null(weights) && !is.numeric(weights)) {
    stop("Weights should be a numeric vector")
  }
  if (!is.null(weights) && length(weights) != original_nrow) {
    stop("Length of weights is not equal to number of observations")
  }

  if (!is.null(weights)) {
    weights <- weights[!navec]
  }


  if (both) {
    yformula <- paste(y, rhs2, sep = " ~ ")
    xformula <- paste(main_expvar, rhs2, sep = " ~ ")
    yres <- lfe::felm(as.formula(yformula), data = data,
                      weights = weights, ...)$residuals
    xres <- lfe::felm(as.formula(xformula), data = data,
                      weights = weights, ...)$residuals

    resdf <- data.frame("y" = yres,
                        "x" = xres)
    colnames(resdf) <- paste0("res_", c(y, main_expvar))
  }else {

    xformula <- paste(main_expvar, rhs2, sep = " ~ ")

    xres <- lfe::felm(as.formula(xformula), data = data,
                      weights = weights, ...)$residuals
    resdf <- data.frame("y" = data[[y]],
                        "x" = xres)
    colnames(resdf) <- c(y, paste0("res_", main_expvar))
  }
  if (!is.null(weights)) {
    resdf <- cbind(resdf, weights)
    colnames(resdf)[3] <- "weights"
  }
  return(resdf)
}
