# nolint start
#' Extracts the partial formulas from the original model
#' plus a vector of all terms to filter the model data.frame before filtering
#' This is an internal function, should not be used on its own
#'
#'
#' Will accept lm, felm (lfe package), and feols (fixest package) objects
#' @title prepare_formula: create partial formulas from an existing model
#' @param model object for which we want to residualise variables
#' @param both if `TRUE` will residualise both the variable of interest and the
#' first explanatory variable in the model. If `FALSE`, only the latter.
#' Set to `TRUE` by default
#' @returns a list with five (four if `both = FALSE`) elements. `filter_terms`,
#' which is a character vector with the column names to select when subsetting
#' the model database. `formulax`, the formula for the main explanatory variable
#' partial model, if `both` is set to `TRUE`, `formulay`, the formula for
#' the variable of interest partial model, `y` for the name of the interest
#' variable, and `x` for the name of the main explanatory variable
#' @srrstats {G2.1} *Function is a generic with S3 methods for all accepted input models*
#' @noRd

# nolint end
prepare_formula <- function(model, both) {
  UseMethod("prepare_formula")
}

#' @importFrom stats formula
#' @importFrom stats terms
prepare_formula.lm <- function(model, both = TRUE) {
  # extract formula from models & terms from formula ----
  f <- formula(model)
  terms_obj <- terms(f)
  # extract interest variable ----
  y <- as.character(attr(terms_obj, "variables"))[2]
  # extract main explanatory variable ----
  x <- attr(terms_obj, "term.labels")[1]
  # extract controls ----
  controls <- attr(terms_obj, "term.labels")[-1]

  if (length(controls) == 0) {
    stop("No control variables found in the model.")
  }

  # check that no poly or asis expressions are used
  if (grepl("I\\(|poly\\(", x) || TRUE %in% grepl("I\\(|poly\\(", controls)) {
    stop("AsIs and poly expressions are not supported")
  }
  # prepare filter terms object ----
  filter_terms <- c(y, x, controls)
  filter_terms <- trimws(unlist(lapply(
    filter_terms,
    strsplit,
    split = "\\:|\\*"
  )))
  filter_terms <- unique(filter_terms)

  # make formulas ----
  if (both) {
    # make formula for y on controls
    formulay <- paste0(y, " ~ ", paste0(controls, collapse = " + "))
    # make formula for 1st x on controls
    formulax <- paste0(x, " ~ ", paste0(controls, collapse = " + "))
    out <- list(
      "filter_terms" = filter_terms,
      "formulay" = formulay,
      "formulax" = formulax,
      "y" = y,
      "x" = x
    )
  } else {
    formulax <- paste0(x, " ~ ", paste0(controls, collapse = " + "))
    out <- list(
      "filter_terms" = filter_terms,
      "formulax" = formulax,
      "y" = y,
      "x" = x
    )
  }
  return(out)
}


#' @importFrom stats formula
#' @importFrom stats terms
prepare_formula.fixest <- function(model, both = TRUE) {
  # extract formula from model and terms from formula ----
  f <- formula(model)
  terms_obj <- terms(f)
  # extract interest variable ----
  y <- as.character(attr(terms_obj, "variables"))[2]
  # extract main explanatory variable and controls / fixed effects ----
  rhs <- attr(terms_obj, "term.labels")

  if (TRUE %in% grepl("I\\(|poly\\(", rhs)) {
    stop("AsIs and poly expressions are not supported")
  }

  # generate the vector of all elements to later subset data.frame ----
  filter_terms <- gsub("i\\(", "", rhs) # remove interactions
  filter_terms <- gsub("\\)", "", filter_terms)
  filter_terms <- trimws(unlist(strsplit(
    filter_terms,
    ",|\\+|\\||\\^|\\[{1,2}|\\]{1,2}"
  )))
  filter_terms <- c(y, filter_terms)
  filter_terms <- unique(filter_terms)

  # get explanatory variables, fe & inst vars ----
  rhs_split <- unlist(strsplit(rhs, "\\|"))
  # main explanatory variable & controls ----
  x <- trimws(rhs_split[1])
  main_expvar <- trimws(unlist(strsplit(x, "\\+")))[1]
  if (grepl("i\\(", main_expvar)) {
    stop("Interaction terms as main explanatory variable are not supported")
  }
  controls <- trimws(unlist(strsplit(x, "\\+")))[-1]
  # fixed effects ----
  fe <- trimws(rhs_split[2])
  # instrumental variables ----
  instvar <- trimws(rhs_split[3])
  if (length(controls) == 0 && is.na(fe)) {
    stop("No fixed effects or control variables found in the model.")
  }

  # make formulas ----

  # prepare common rhs to both partial equations
  rhs2 <- if (length(controls) == 0) {
    c("1", fe, instvar)
  } else {
    c(controls, fe, instvar)
  }
  rhs2 <- rhs2[!is.na(rhs2)]
  rhs2 <- paste0(rhs2, collapse = " | ")

  if (both) {
    formulay <- paste(y, rhs2, sep = " ~ ")
    formulax <- paste(main_expvar, rhs2, sep = " ~ ")
    out <- list(
      "filter_terms" = filter_terms,
      "formulay" = formulay,
      "formulax" = formulax,
      "y" = y,
      "x" = main_expvar
    )
  } else {
    formulax <- paste(main_expvar, rhs2, sep = " ~ ")

    out <- list(
      "filter_terms" = c(y, x, controls),
      "formulax" = formulax,
      "y" = y,
      "x" = main_expvar
    )
  }
  return(out)
}

#' @importFrom stats formula
#' @importFrom stats terms
prepare_formula.felm <- function(model, both = TRUE) {
  # extract formula from model and terms from formula ----
  f <- formula(model)
  terms_obj <- terms(f)
  # extract interest variable ----
  y <- as.character(attr(terms_obj, "variables"))[2]
  # extract main explanatory variable and controls / fixed effects ----
  rhs <- attr(terms_obj, "term.labels")

  if (TRUE %in% grepl("I\\(|poly\\(", rhs)) {
    stop("AsIs and poly expressions are not supported")
  }

  # generate the vector of all elements to later subset data.frame
  filter_terms <- trimws(unlist(strsplit(rhs, "\\+|\\||\\*|\\:")))
  filter_terms <- c(y, filter_terms)
  # get explanatory variables, fe, inst vars, and error clusters ----
  rhs_split <- unlist(strsplit(rhs, "\\|"))
  # main explanatory variable and controls ----
  x <- trimws(rhs_split[1]) # indep variables
  main_expvar <- trimws(unlist(strsplit(x, "\\+")))[1]
  controls <- trimws(unlist(strsplit(x, "\\+")))[-1]

  # fixed effects ----
  fe <- trimws(rhs_split[2])
  # instrumental variables ----
  instvar <- trimws(rhs_split[3])
  # error clusters ----
  cluster <- trimws(rhs_split[4])
  if (length(controls) == 0 && is.na(fe)) {
    stop("No fixed effects or control variables found in the model.")
  }

  # make formulas ----

  rhs2 <- if (length(controls) == 0) {
    c("1", fe, instvar, cluster)
  } else {
    c(controls, fe, instvar, cluster)
  }
  rhs2 <- rhs2[!is.na(rhs2)]
  rhs2 <- paste0(rhs2, collapse = " | ")

  # create formulas

  if (both) {
    formulay <- paste(y, rhs2, sep = " ~ ")
    formulax <- paste(main_expvar, rhs2, sep = " ~ ")
    out <- list(
      "filter_terms" = filter_terms,
      "formulay" = formulay,
      "formulax" = formulax,
      "y" = y,
      "x" = main_expvar
    )
  } else {
    formulax <- paste(main_expvar, rhs2, sep = " ~ ")

    out <- list(
      "filter_terms" = c(y, x, controls),
      "formulax" = formulax,
      "y" = y,
      "x" = main_expvar
    )
  }

  return(out)
}
