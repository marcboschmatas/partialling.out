
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
#' @param weights a numeric vector for weighting the partial models. Length must be
#' equal to number of rows of `data`
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
#' @srrstats {G2.0} *An assertion has been included to make sure length of the original data.frame and weights match.*
#' @srrstats {G2.0a} *Explicit documentation on length of `weights` has been added*
#' @srrstats {G2.1} *Assertions on input types implemented via S3 methods + conditionals*
#' @srrstats {G2.1a} *Explicit secondary documentation of expectations on data types of all vector inputs provided.*
#' @srrstats {G2.14} *Options for NA handling provided in `na.rm`. Further `na.option` arguments to underlying functions can be passed in `...`*
#' @srrstats {G2.13} *Checks for missing data implemented*
#' @srrstats {G2.14a} *Options for NA handling provided in `na.rm`, and `na.option` arguments to underlying functions can be passed in `...`*
#' @srrstats {G2.14b} *Options for NA handling provided in `na.rm`, and `na.option` arguments to underlying functions can be passed in `...`*
#' @srrstats {G2.15} *Non missingness is not assumed and passing data with missing values may cause errors or unexpected behaviour*
#' @srrstats {G2.16} *undefined values (e.g., `NaN`, `Inf` and `-Inf`) are handled by underlying model functions*
#' @srrstats {G3.0} *All comparisons are made within ranges of tolerance.*
#' @srrstats {G5.8a} *Zero-length data will throw errors*
#' @srrstats {G5.8b} *Data of unsupported types will throw errors*
#' @srrstats {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns will throw errors*
#' @srrstats {G5.8d} *Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)*
#' @srrstats {G2.10} *Filtering works via removal of NA values if `na.rm = TRUE`*
#' @srrstats {EA2.6} *Vector data with additional attributes will be transformed via the underlying models*
#' @srrstats {EA3.0} *The only implementation of FWL theorem other than manually implementing it includes passing a formula to a function instead of the usual model.*
#' @srrstats {EA3.1} *Applying the FWL theorem to a regression model can only be done via plotting (using `fwlplot`) or manually defining functions for it.*
#' @srrstats {EA4.0} *Software returns a data.frame of residualised variables from a regression model*
#' @srrstats {EA4.1} *EDA Software should implement parameters to enable explicit control of numeric precision*
#' @srrstats {EA4.2} *Software returns a data.frame, with appropriate `print()`, and `plot()` methods*
#' @srrstats {EA5.0} *Package does not provide visualisations, but vignettes have been designed to ensure accessibility*
#' @srrstats {EA5.0a} *Typeface sizes for vignettes have been designed to ensure accessibility*
#' @srrstats {EA5.0b} *Default colour schemes for vignettes have been designed to ensure accessibility*
#' @srrstats {EA5.1} *No typefaces overriding*
#' @srrstats {EA5.2} *Printing of numeric values is done via `print.data.frame()` which includes rounding.*
#' @srrstats {G2.7} *Software accepts extensions to data.frame*
#' @srrstats {G2.12} *Software accepts and transforms list columns if the underlying model is `feols()` and will throw an error if it is `lm()` or `felm()`*
#' @srrstats {G2.11} *`data.frame`-like tabular objects which have columns which do not themselves have standard class attributes (typically, `vector`) are appropriately processed*
#' @srrstats {G2.6} *One-dimensional input (weights) is appropriately pre-processed regardless of class structures.*
#' @srrstats {G2.8} *Data.frame is maintained as such, and formulas are transformed into partial formulas via the retrieval and modification of attributes*
#' @srrstats {G2.9} *If potential loss of information can be found, package will throw a warning*

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
