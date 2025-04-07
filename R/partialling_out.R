
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
#' @srrstats {G5.8a} *Zero-length data*
#' @srrstats {G5.8b} *Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data)*
#' @srrstats {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns*
#' @srrstats {G5.8d} *Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)*
#' @srrstats {G2.10} *Software should ensure that extraction or filtering of single columns from tabular inputs should not presume any particular default behaviour, and should ensure all column-extraction operations behave consistently regardless of the class of tabular data used as input.*
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
#' @srrstats {G2.7} *Software should accept as input as many of the above standard tabular forms as possible, including extension to domain-specific forms.*
#' @srrstats {G2.12} *Software should ensure that `data.frame`-like tabular objects which have list columns should ensure that those columns are appropriately pre-processed either through being removed, converted to equivalent vector columns where appropriate, or some other appropriate treatment such as an informative error. This behaviour should be tested.*
#' @srrstats {G2.11} *Software should ensure that `data.frame`-like tabular objects which have columns which do not themselves have standard class attributes (typically, `vector`) are appropriately processed, and do not error without reason. This behaviour should be tested. Again, columns created by the [`units` package](https://github.com/r-quantities/units/) provide a good test case.*
#' @srrstats {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
#' @srrstats {G2.8} *Software should provide appropriate conversion or dispatch routines as part of initial pre-processing to ensure that all other sub-functions of a package receive inputs of a single defined class or type.*
#' @srrstats {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).*

#' @export
# nolint end
partialling_out <- function(model, data, weights, both, na.rm, ...) {
  UseMethod("partialling_out")
}


#' @importFrom stats lm
#' @importFrom stats as.formula
#' @export
partialling_out.lm <- function(model, data = NULL,
                               weights = NULL, both = TRUE,
                               na.rm = TRUE, ...) {

  # prepare partial formulas and filter terms ----

  formulas <- prepare_formula(model = model,
                              both = both)

  # check data is provided and is a data.frame ----

  if (is.null(data)) {
    stop("No data has been provided")
  } # check data was provided
  if (!("data.frame" %in% class(data))) {
    stop("data input should be a data.frame")
  } # check data is a df

  # subset and filter data.frame ----
  # subset
  data <- data[, formulas$filter_terms]

  # set warning if column class attributes are not standard
  classes <- vapply(data, \(x) class(x)[1], FUN.VALUE = character(1))


  if (length(classes[!(classes %in% c("numeric", "character", "factor",
                                      "Date", "POSIXct", "POSIXt",
                                      "datetime", "logical"))]) > 0) {
    warning("One or more columns have non standard classes")
  }

  original_nrow <- nrow(data) # for check later


  ## remove NA if needed ----


  if (na.rm) {
    navec <- apply(data,
                   1,
                   function(x) any(is.na(x)))
    data <- data[!navec, ]
  }else {
    warning("na.rm is set to FALSE and results depend on lm() na_action")
  }

  # prepare weights ----

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

  # generate residuals ----

  if (both) {

    resy <- lm(as.formula(formulas$formulay),
               data = data, weights = weights, ...)$residuals
    resx <- lm(as.formula(formulas$formulax),
               data = data, weights = weights, ...)$residuals
    resdf <- data.frame("y" = resy,
                        "x" = resx)

    colnames(resdf) <- paste0("res_", c(formulas$y, formulas$x))
  }else {
    resx <- lm(as.formula(formulas$formulax), data = data,
               eights = weights, ...)$residuals
    resdf <- data.frame("y" = data[[formulas$y]],
                        "x" = resx)
    colnames(resdf) <- c(formulas$y, paste0("res_", formulas$x))
  }
  if (!is.null(weights)) {
    resdf <- cbind(resdf, weights)
    colnames(resdf)[3] <- "weights"
  }

  return(resdf)
}


#' @importFrom stats lm
#' @importFrom stats as.formula
#' @importFrom fixest feols
#' @export
partialling_out.fixest <- function(model, data = NULL,
                                   weights = NULL, both = TRUE,
                                   na.rm = TRUE, ...) {
  # prepare partial formulas and filter terms ----

  formulas <- prepare_formula(model = model,
                              both = both)

  # check data is provided and is a data.frame ----


  if (is.null(data)) {
    stop("No data has been provided")
  }  # check that a data object is supplied
  if (!("data.frame" %in% class(data))) {
    stop("data input should be a data.frame")
  } # check it is a data.frame


  # subset and filter data.frame ----
  # subset
  filter_terms <- formulas$filter_terms
  filter_terms <- filter_terms[filter_terms %in% colnames(data)]
  data <- data[, filter_terms]

  # throw a warning if columns have non standard class attributes
  classes <- vapply(data, \(x) class(x)[1], FUN.VALUE = character(1))


  if (length(classes[!(classes %in% c("numeric", "character", "factor",
                                      "Date", "POSIXct", "POSIXt",
                                      "datetime", "logical"))]) > 0) {
    warning("One or more columns have non standard classes")
  }



  ## remove NA if needed ----
  original_nrow <- nrow(data) # for check later
  if (na.rm) {
    navec <- apply(data, 1, function(x) any(is.na(x))
    )
    data <- data[!navec, ]
  }else {
    warning("na.rm is set to FALSE and results depend on feols() na_action")
  }


  # prepare weights ----

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

  # generate residuals ----

  if (both) {
    yres <- fixest::feols(as.formula(formulas$formulay), data = data,
                          weights = weights, ...)$residuals
    xres <- fixest::feols(as.formula(formulas$formulax), data = data,
                          weights = weights, ...)$residuals

    resdf <- data.frame("y" = yres,
                        "x" = xres)
    colnames(resdf) <- paste0("res_", c(formulas$y, formulas$x))
  }else {

    xres <- fixest::feols(as.formula(formulas$formulax), data = data,
                          weights = weights, ...)$residuals

    resdf <- data.frame("y" = data[[formulas$y]],
                        "x" = xres)
    colnames(resdf) <- c(formulas$y, paste0("res_", formulas$x))
  }
  if (!is.null(weights)) {
    resdf <- cbind(resdf, weights)
    colnames(resdf)[3] <- "weights"
  }


  return(resdf)
}


#' @importFrom lfe felm
#' @importFrom stats as.formula
#' @export
partialling_out.felm <- function(model, data = NULL,
                                 weights = NULL, both = TRUE,
                                 na.rm = TRUE, ...) {

  # prepare partial formulas and filter terms ----

  formulas <- prepare_formula(model = model,
                              both = both)

  # check data is provided and is a data.frame ----

  if (is.null(data)) {
    stop("No data has been provided")
  } # check that a data object is supplied
  if (!("data.frame" %in% class(data))) {
    stop("data input should be a data.frame")
  } # check data is a data.frame

  # subset and filter data.frame

  # subset and filter data ----
  # subset
  filter_terms <- formulas$filter_terms
  filter_terms <- filter_terms[filter_terms %in% colnames(data)]
  data <- data[, filter_terms]

  # warning if columns have non standard class attributes
  classes <- vapply(data, \(x) class(x)[1], FUN.VALUE = character(1))


  if (length(classes[!(classes %in% c("numeric", "character", "factor",
                                      "Date", "POSIXct", "POSIXt",
                                      "datetime", "logical"))]) > 0) {
    warning("One or more columns have non standard classes")
  }


  ## remove NA if needed ----
  original_nrow <- nrow(data) # later check

  if (na.rm) {
    navec <- apply(data, 1, function(x) any(is.na(x)))

    data <- data[!navec, ]
  }else {
    warning("na.rm is set to FALSE and results depend on felm() na_action")
  }

  # prepare weights ----
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

  # generate residuals ----

  if (both) {
    yres <- lfe::felm(as.formula(formulas$formulay), data = data,
                      weights = weights, ...)$residuals
    xres <- lfe::felm(as.formula(formulas$formulax), data = data,
                      weights = weights, ...)$residuals

    resdf <- data.frame("y" = yres,
                        "x" = xres)
    colnames(resdf) <- paste0("res_", c(formulas$y, formulas$x))
  }else {


    xres <- lfe::felm(as.formula(formulas$formulax), data = data,
                      weights = weights, ...)$residuals
    resdf <- data.frame("y" = data[[formulas$y]],
                        "x" = xres)
    colnames(resdf) <- c(formulas$y, paste0("res_", formulas$x))
  }
  if (!is.null(weights)) {
    resdf <- cbind(resdf, weights)
    colnames(resdf)[3] <- "weights"
  }
  return(resdf)
}
