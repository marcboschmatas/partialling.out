

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
#' @param data data.frame used in the original model
#' @param weights a numeric vector for weighting the partial models
#' @param both if `TRUE` will residualise both the variable of interest and the
#' first explanatory variable in the model. If `FALSE`, only the latter.
#' Set to `TRUE` by default
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
#' @export
partialling_out <- function(model, data, weights, both, ...) {
  UseMethod("partialling_out")
}

#' @importFrom stats formula
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom stats terms
#' @export
partialling_out.lm <- function(model, data = NULL,
                               weights = NULL, both = TRUE, ...) {
  if (is.null(data)) {
    stop("No data has been provided")
  } # check data was provided
  if (!("data.frame" %in% class(data))) {
    stop("data input should be a data.frame")
  } # check data was provided
  # get formula
  f <- formula(model)
  terms_obj <- terms(f)
  y <- as.character(attr(terms_obj, "variables"))[2]
  x <-  attr(terms_obj, "term.labels")[1]
  controls <- attr(terms_obj, "term.labels")[-1]
  if (length(controls) == 0) {
    stop("No control variables found in the model.")
  }

  # get residuals
  data <- data[, c(y, x, controls)]
  original_nrow <- nrow(data) # for check later
  navec <- apply(data,
                 1,
                 function(x) any(is.na(x)))
  data <- data[!navec, ]
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

  return(resdf)
}

#' @importFrom stats formula
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom fixest feols
#' @export
partialling_out.fixest <- function(model, data = NULL,
                                   weights = NULL, both = TRUE, ...) {
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
  data <- data[, c(y, terms_filter)]
  original_nrow <- nrow(data) # for check later
  navec <- apply(data, 1, function(x) any(is.na(x))
  )
  data <- data[!navec, ]
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
  if (!is.null(weights)) {
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


  return(resdf)
}

#' @importFrom stats formula
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom lfe felm
#' @export
partialling_out.felm <- function(model, data = NULL,
                                 weights = NULL, both = TRUE, ...) {
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
  data <- data[, c(y, terms_filter)]
  navec <- apply(data, 1, function(x) any(is.na(x)))
  original_nrow <- nrow(data) # later check
  data <- data[!navec, ]
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
  return(resdf)
}
