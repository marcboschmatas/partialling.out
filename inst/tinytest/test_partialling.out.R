library(palmerpenguins)
library(fixest)
library(lfe)
library(tinytest)
library(tinysnapshot)
library(partialling.out)
library(tsibble)
library(units)
library(purrr)
library(tinyplot)
source("helpers.R") # helpers for snapshot test


penguins <- penguins
set.seed(1234)

# general standards for tests

# nolint start
#' @srrstats {G5.0} *Tests use the `palmerpenguins` dataset which is available on CRAN or a purpose-made dataset created under a random seed.*
#' @srrstats {G5.1} *All datasets are available to the end user, either by being created within the tests file or available on CRAN*
#' @srrstats {G5.3} *The absence of NA, NaN, or inf values is properly tested.*
NULL
# nolint end

# Test for errors warnings etc ----

# nolint start
#' @srrstats {G5.2} *All possible error conditions are tested*
#' @srrstats {G5.2a} *All error or warning messages are unique*
#' @srrstats {G5.2b} *All error and warning messages have been checked*
NULL

# nolint end

## Expect error if no data is provided ----

### lm ----
safe_partialling_out <- safely(partialling_out)


mod <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)


lmerrornodata <- safe_partialling_out(mod)

expect_equal(lmerrornodata$error$message, "No data has been provided")


### feols ----

mod <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)

feolserrornodata <- safe_partialling_out(mod)

expect_equal(feolserrornodata$error$message, "No data has been provided")


### felm----
mod <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)

felmerrornodata <- safe_partialling_out(mod)

expect_equal(felmerrornodata$error$message, "No data has been provided")

## expect error if data is not a data.frame ----

### lm ----
mod <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

lmerrornodf <- safe_partialling_out(mod, data = c(1, 2, 3))

expect_equal(lmerrornodf$error$message, "data input should be a data.frame")


### feols ----

mod <- feols(bill_length_mm ~ bill_depth_mm + species, data = penguins)

feolserrornodf <- safe_partialling_out(mod, data = c(1, 2, 3))

expect_equal(feolserrornodf$error$message, "data input should be a data.frame")


### felm ----

mod <- felm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

felmerrornodf <- safe_partialling_out(mod, data = c(1, 2, 3))

expect_equal(felmerrornodf$error$message, "data input should be a data.frame")


## expect error if weights are non numeric ----

### lm ----
mod <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

nonnumweightslm <- safe_partialling_out(
  mod,
  data = penguins,
  weights = rep("a", nrow(penguins))
)

expect_equal(
  nonnumweightslm$error$message,
  "Weights should be a numeric vector"
)

### feols ----

mod <- feols(bill_length_mm ~ bill_depth_mm + species, data = penguins)

nonnumweightsfeols <- safe_partialling_out(
  mod,
  data = penguins,
  weights = rep("a", nrow(penguins))
)

expect_equal(
  nonnumweightsfeols$error$message,
  "Weights should be a numeric vector"
)
### felm ----

mod <- felm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

nonnumweightsfelm <- safe_partialling_out(
  mod,
  data = penguins,
  weights = rep("a", nrow(penguins))
)

expect_equal(
  nonnumweightsfelm$error$message,
  "Weights should be a numeric vector"
)
## expect error if weights are of the wrong length ----

### lm ----
mod <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

wrongerrorslm <- safe_partialling_out(mod, penguins, weights = c(1, 2, 3))

expect_equal(
  wrongerrorslm$error$message,
  "Length of weights is not equal to number of observations"
)
### feols ----

mod <- feols(bill_length_mm ~ bill_depth_mm + species, data = penguins)

wrongerrorsfeols <- safe_partialling_out(mod, penguins, weights = c(1, 2, 3))

expect_equal(
  wrongerrorsfeols$error$message,
  "Length of weights is not equal to number of observations"
)


### felm ----

mod <- felm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

wrongerrorsfelm <- safe_partialling_out(mod, penguins, weights = c(1, 2, 3))

expect_equal(
  wrongerrorsfelm$error$message,
  "Length of weights is not equal to number of observations"
)

## expect error if only one covariate or no fixed effects ----

### lm ----
mod <- lm(bill_length_mm ~ bill_depth_mm, data = penguins)

onecovlm <- safe_partialling_out(mod, penguins)

expect_equal(onecovlm$error$message, "No control variables found in the model.")
### feols----

mod <- feols(bill_length_mm ~ bill_depth_mm, data = penguins)

onecovfeols <- safe_partialling_out(mod, penguins)

expect_equal(
  onecovfeols$error$message,
  "No fixed effects or control variables found in the model."
)


### felm ----

mod <- felm(bill_length_mm ~ bill_depth_mm, data = penguins)

onecovfelm <- safe_partialling_out(mod, penguins)

expect_equal(
  onecovfelm$error$message,
  "No fixed effects or control variables found in the model."
)


# nolint start
#' @srrstats {G5.8a} *Zero-length data will throw errors*
#' @srrstats {G5.8b} *Data of unsupported types will throw errors*
#' @srrstats {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns will throw errors*
NULL

# nolint end

## expect error if zero length data is provided ----

### lm----
mod <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

noldatalm <- safe_partialling_out(mod, penguins[0, ])

expect_equal(noldatalm$error$message, "data has zero rows")
### feols ----

mod <- feols(bill_length_mm ~ bill_depth_mm + species, data = penguins)

noldatafeols <- safe_partialling_out(mod, penguins[0, ])

expect_equal(noldatafeols$error$message, "data has zero rows")


### felm----

mod <- felm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

noldatafelm <- safe_partialling_out(mod, penguins[0, ])

expect_equal(noldatafelm$error$message, "data has zero rows")


## expect error if wrong data types are used ----

penguins_wrong <- penguins

penguins_wrong$bill_length_mm <- rep("a", nrow(penguins))
penguins_wrong$bill_depth_mm <- rep("b", nrow(penguins))


### lm ----

mod <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

wrongdata <- safe_partialling_out(mod, data = penguins_wrong)

expect_equal(wrongdata$error$message, "Y or X cannot be converted to numeric")
### feols ----

mod <- feols(bill_length_mm ~ bill_depth_mm + species, data = penguins)

wrongdata <- safe_partialling_out(mod, data = penguins_wrong)

expect_equal(wrongdata$error$message, "Y or X cannot be converted to numeric")

### felm ----

mod <- felm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

wrongdata <- safe_partialling_out(mod, data = penguins_wrong)

expect_equal(wrongdata$error$message, "Y or X cannot be converted to numeric")


## expect error if all variables are NA ----

df1 <- data.frame("y" = rnorm(1000), "x1" = rnorm(1000), "x2" = rnorm(1000))
nadf <- data.frame(
  "y" = rep(NA, 1000),
  "x1" = rep(NA, 1000),
  "x2" = rep(NA, 1000)
)


### lm ----
mod <- lm(y ~ x1 + x2, data = df1)

nadata <- safe_partialling_out(mod, nadf)

expect_equal(nadata$error$message, "Y or X cannot be converted to numeric")
### feols ----

mod <- feols(y ~ x1 + x2, data = df1)

nadata <- safe_partialling_out(mod, nadf)

expect_equal(nadata$error$message, "Y or X cannot be converted to numeric")
### felm ----

mod <- felm(y ~ x1 + x2, data = df1)

nadata <- safe_partialling_out(mod, nadf)

expect_equal(nadata$error$message, "Y or X cannot be converted to numeric")


## expect a warning if partial models are weighted but original model is not ----

quiet_partialling_out <- quietly(partialling_out)


### lm ----
mod <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

weightswarning <- quiet_partialling_out(
  mod,
  penguins,
  weights = penguins$body_mass_g
)

expect_equal(
  weightswarning$warnings,
  "Original model is not weighted, consider if weights are necessary"
)

### feols ----
mod <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)


weightswarning <- quiet_partialling_out(
  mod,
  penguins,
  weights = penguins$body_mass_g
)

expect_equal(
  weightswarning$warnings,
  "Original model is not weighted, consider if weights are necessary"
)

### felm ----

mod <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)


weightswarning <- quiet_partialling_out(
  mod,
  penguins,
  weights = penguins$body_mass_g
)

expect_equal(
  weightswarning$warnings,
  "Original model is not weighted, consider if weights are necessary"
)

## expect a warning if original model is weighted but not partialling_out ----

### lm ----
mod <- lm(
  bill_length_mm ~ bill_depth_mm + species,
  data = penguins,
  weights = penguins$body_mass_g
)


weightswarning <- quiet_partialling_out(mod, penguins)

expect_equal(
  weightswarning$warnings,
  "Original model is weighted, consider if weights should be added"
)


### feols ----

mod <- feols(
  bill_length_mm ~ bill_depth_mm + species,
  data = penguins,
  weights = penguins$body_mass_g
)


weightswarning <- quiet_partialling_out(mod, penguins)

expect_equal(
  weightswarning$warnings,
  "Original model is weighted, consider if weights should be added"
)

### felm ----

nonapenguins <- penguins[!is.na(penguins$body_mass_g), ]


mod <- felm(
  bill_length_mm ~ bill_depth_mm + species,
  data = nonapenguins,
  weights = nonapenguins$body_mass_g
)


weightswarning <- quiet_partialling_out(mod, nonapenguins)

expect_equal(
  weightswarning$warnings,
  "Original model is weighted, consider if weights should be added"
)


## expect error if weights are included but na are not omitted----

### lm ----

model <- lm(
  bill_length_mm ~ bill_depth_mm + species,
  data = penguins,
  weights = penguins$body_mass_g
)


nonarmerror <- safe_partialling_out(
  model,
  penguins,
  weights = penguins$body_mass_g,
  na.rm = FALSE
)

expect_equal(
  nonarmerror$error$message,
  "arguments imply differing number of rows: 342, 344"
)

### feols ----

model <- feols(
  bill_length_mm ~ bill_depth_mm + species,
  data = penguins,
  weights = penguins$body_mass_g
)


nonarmerror <- safe_partialling_out(
  model,
  penguins,
  weights = penguins$body_mass_g,
  na.rm = FALSE
)

expect_equal(
  nonarmerror$error$message,
  "arguments imply differing number of rows: 342, 344"
)


# felm will not allow a model with missing weights, so not testing

## test that no error will return if "0" elements are included in felm ----

model <- felm(
  bill_length_mm ~ bill_depth_mm | species | 0 | species,
  data = penguins
)

expect_silent(partialling_out(model, data = penguins))

## test that column summaries will behave as expected ----

# nolint start
#' @srrstats {EA5.3} *summaries are tested to behave as expected*
NULL
# nolint end

### lm ----

model <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)
res <- partialling_out(model, data = penguins)

summaries <- lapply(res, summary)

sumnames <- unique(unlist(lapply(summaries, names)))

expect_equal(
  sumnames,
  c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
)

### feols ----

model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)


summaries <- lapply(res, summary)

sumnames <- unique(unlist(lapply(summaries, names)))

expect_equal(
  sumnames,
  c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
)


### fellm ----

model <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)


summaries <- lapply(res, summary)

sumnames <- unique(unlist(lapply(summaries, names)))

expect_equal(
  sumnames,
  c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
)


## test that a non standard dataframe will be accepted----

# nolint start
#' @srrstats {G2.7} *`tsibble` objects are accepted and treted like usual data.frames*
NULL
# nolint end

n <- 100L
df <- tsibble::tsibble(
  date = as.Date("2017-01-01") + seq_len(n),
  dv = rnorm(n),
  iv1 = rnorm(n),
  iv2 = sample(letters, size = n, replace = TRUE),
  iv3 = I(as.list(sample(letters, size = n, replace = TRUE))),
  iv4 = units::set_units(rnorm(n), "m/s")
)

### lm ----

model <- lm(dv ~ iv1 + iv2, data = df)

expect_silent(partialling_out(model, df))

### feols ----

model <- feols(dv ~ iv1 | iv2, data = df)

expect_silent(partialling_out(model, df))


### felm ----

model <- felm(dv ~ iv1 + iv2, data = df)

expect_silent(partialling_out(model, df))


# #test that columns with weird classes will throw a warning  ----
# nolint start
#' @srrstats {G2.11} *columns with non standard classes are tested to be accepted and that a warning is given that conversion is up to the underlying model*
NULL
# nolint end

model <- lm(dv ~ iv1 + iv4 + iv2, data = df)

expect_warning(partialling_out(model, df))


model <- felm(dv ~ iv1 + iv4 | iv2, data = df)

expect_warning(partialling_out(model, df))


## #further check that list columns are accepted in partialling_out.feols ----
# nolint start
#' @srrstats {G2.12} *list-columns are tested to be accepted in `feols`*
NULL
# nolint end

model <- feols(dv ~ iv1 | iv2 + iv3, data = df)

expect_warning(partialling_out(model, df))

## expect error if interaction terms with i() are passed in fixest

m <- feols(bill_length_mm ~ i(species, bill_depth_mm) | sex, data = penguins)

interacterror <- safe_partialling_out(m, penguins)

expect_equal(
  interacterror$error$message,
  "Interaction terms as main explanatory variable are not supported"
)


## expect error if AsIs expressions are used ----

### lm ----

m <- lm(bill_length_mm ~ I(bill_depth_mm^2) + species, data = penguins)

asiserror <- safe_partialling_out(m)

expect_equal(
  asiserror$error$message,
  "AsIs and poly expressions are not supported"
)

### feols ----

m <- feols(bill_length_mm ~ I(bill_depth_mm^2) | species, data = penguins)

asiserror <- safe_partialling_out(m)

expect_equal(
  asiserror$error$message,
  "AsIs and poly expressions are not supported"
)
### felm ----

m <- felm(bill_length_mm ~ I(bill_depth_mm^2) | species, data = penguins)

asiserror <- safe_partialling_out(m)

expect_equal(
  asiserror$error$message,
  "AsIs and poly expressions are not supported"
)

## expect error if poly expressions are used ----

penguins_nomis <- penguins[!is.na(penguins$bill_depth_mm), ]

### lm ----

m <- lm(
  bill_length_mm ~ poly(bill_depth_mm, 2) + species,
  data = penguins_nomis
)

polyerror <- safe_partialling_out(m)

expect_equal(
  polyerror$error$message,
  "AsIs and poly expressions are not supported"
)

### feols ----

m <- feols(
  bill_length_mm ~ poly(bill_depth_mm, 2) | species,
  data = penguins_nomis
)

polyerror <- safe_partialling_out(m)

expect_equal(
  polyerror$error$message,
  "AsIs and poly expressions are not supported"
)
### felm ----

m <- felm(
  bill_length_mm ~ poly(bill_depth_mm, 2) | species,
  data = penguins_nomis
)

polyerror <- safe_partialling_out(m)

expect_equal(
  polyerror$error$message,
  "AsIs and poly expressions are not supported"
)

## test that interaction terms are broken properly ----

m2 <- feols(bill_length_mm ~ bill_depth_mm | sex^species, data = penguins)

expect_silent(partialling_out(m2, penguins))

m3 <- feols(
  bill_length_mm ~ bill_depth_mm | sex[flipper_length_mm],
  data = penguins
)

expect_warning(partialling_out(m3, penguins)) # this will throw a warning because of interaction terms

m4 <- feols(
  bill_length_mm ~ bill_depth_mm | sex[[flipper_length_mm]],
  data = penguins
)

expect_warning(partialling_out(m4, penguins)) # this will throw a warning because of interaction terms


### lm ----

m <- lm(bill_length_mm ~ bill_depth_mm * sex + species, data = penguins)

expect_silent(partialling_out(m, penguins))

m2 <- lm(bill_length_mm ~ bill_depth_mm:sex + species, data = penguins)


expect_warning(partialling_out(m2, penguins))


# test for results validity ----

# nolint start
#' @srrstats {G5.4} *Correctness tests are implemented*
#' @srrstats {G5.5} *Correctness tests are run with a fixed random seed*
#' @srrstats {G5.6} *Correctness tests are performed with a well-known dataset*
#' @srrstats {G5.6a} *Correctness test is applied within a tolerance band*
#' @srrstats {G5.6b} *Parameter recovery tests are run with multiple random seeds when either data simulation or the algorithm contains a random component.*
#' @srrstats {G5.7} **Algorithm performance tests** *to test that implementation performs as expected as properties of data change. For instance, a test may show that parameters approach correct estimates within tolerance as data size increases, or that convergence times decrease for higher convergence thresholds.*
#' @srrstats {G5.8} **Edge condition tested*
#' @srrstats {G5.9} **Noise susceptibility tested*
#' @srrstats {G5.9a} *Adding trivial noise (for example, at the scale of `.Machine$double.eps`) to data does not meaningfully change results has been tested*
#' @srrstats {G5.9b} *Running under different random seeds or initial conditions does not meaningfully change results. this behaviour has been tested*

NULL
# nolint end
## lm ----
### test that fwl theorem is properly applied ----

model <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm, data = res)

expect_equal(
  unname(signif(model$coefficients[2], 4)),
  unname(signif(resmod$coefficients[2], 4))
)

### check that a different random seed will provide the same results ----

set.seed(5678)

model2 <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)
res2 <- partialling_out(model2, data = penguins)

resmod2 <- lm(res_bill_length_mm ~ res_bill_depth_mm, data = res)

expect_equal(
  unname(signif(resmod$coefficients[2], 4)),
  unname(signif(resmod2$coefficients[2], 4))
)


### check that random noise does not meaningfully change results ----

penguins$bill_length_mm2 <- penguins$bill_length_mm + .Machine$double.eps
penguins$bill_depth_mm2 <- penguins$bill_depth_mm + .Machine$double.eps


model3 <- lm(bill_length_mm2 ~ bill_depth_mm2 + species, data = penguins)
res <- partialling_out(model3, data = penguins)

resmod3 <- lm(res_bill_length_mm2 ~ res_bill_depth_mm2, data = res)

expect_equal(
  unname(signif(resmod$coefficients[2], 4)),
  unname(signif(resmod3$coefficients[2], 4))
)


## feols ----
### test that fwl theorem is properly applied ----

set.seed(1234)

model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm, data = res)

expect_equal(
  unname(signif(model$coefficients[1], 4)),
  unname(signif(resmod$coefficients[2], 4))
)


### check that a different random seed will provide the same results ----

set.seed(5678)

model2 <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res2 <- partialling_out(model2, data = penguins)

resmod2 <- lm(res_bill_length_mm ~ res_bill_depth_mm, data = res)

expect_equal(
  unname(signif(resmod$coefficients[2], 4)),
  unname(signif(resmod2$coefficients[2], 4))
)

penguins$bill_length_mm2 <- penguins$bill_length_mm + .Machine$double.eps
penguins$bill_depth_mm2 <- penguins$bill_depth_mm + .Machine$double.eps


### check that random noise does not meaningfully change results ----

model3 <- feols(bill_length_mm2 ~ bill_depth_mm2 + species, data = penguins)
res <- partialling_out(model3, data = penguins)

resmod3 <- lm(res_bill_length_mm2 ~ res_bill_depth_mm2, data = res)

expect_equal(
  unname(signif(resmod$coefficients[2], 4)),
  unname(signif(resmod3$coefficients[2], 4))
)


## felm ----
### test that fwl theorem is properly applied ----

set.seed(1234)

model <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm, data = res)

expect_equal(
  unname(signif(model$coefficients[1], 4)),
  unname(signif(resmod$coefficients[2], 4))
)

### check that a different random seed will provide the same results ----

set.seed(5678)

model2 <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res2 <- partialling_out(model2, data = penguins)

resmod2 <- lm(res_bill_length_mm ~ res_bill_depth_mm, data = res)

expect_equal(
  unname(signif(resmod$coefficients[2], 4)),
  unname(signif(resmod2$coefficients[2], 4))
)


### check that random noise does not meaningfully change results ----

model3 <- felm(bill_length_mm2 ~ bill_depth_mm2 + species, data = penguins)
res <- partialling_out(model3, data = penguins)

resmod3 <- lm(res_bill_length_mm2 ~ res_bill_depth_mm2, data = res)

expect_equal(
  unname(signif(resmod$coefficients[2], 4)),
  unname(signif(resmod3$coefficients[2], 4))
)


# test for return object characteristics ----

# nolint start
#' @srrstats {EA6.0} *Return values from all functions are tested*
#' @srrstats {EA6.0a} *Classes and types of objects are tested*
#' @srrstats {EA6.0b} *number of columns is tested*
#' @srrstats {EA6.0c} *Column names (or equivalent) of tabular objects are tested*
#' @srrstats {EA6.0d} *Classes or types of all columns contained within `data.frame`-type tabular objects are tested*
NULL
# nolint end

## test that class from return object are partial_residuals and data.frame ----
model <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)
res <- partialling_out(model, data = penguins)

expect_true("data.frame" %in% class(res))
expect_true("partial_residuals" %in% class(res))

model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

expect_true("data.frame" %in% class(res))
expect_true("partial_residuals" %in% class(res))


model <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

expect_true("data.frame" %in% class(res))
expect_true("partial_residuals" %in% class(res))


## test that it returns a data.frame of two columns if unweighted ----
model <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)
res <- partialling_out(model, data = penguins)

expect_true(ncol(res) == 2)

model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

expect_true(ncol(res) == 2)

model <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

expect_true(ncol(res) == 2)


## check that it returns three columns if weighted ----

model <- lm(
  bill_length_mm ~ bill_depth_mm + species,
  data = penguins,
  weights = penguins$body_mass_g
)
res <- partialling_out(model, data = penguins, weights = penguins$body_mass_g)

expect_true(ncol(res) == 3)

model <- feols(
  bill_length_mm ~ bill_depth_mm | species,
  data = penguins,
  weights = penguins$body_mass_g
)
res <- partialling_out(model, data = penguins, weights = penguins$body_mass_g)

expect_true(ncol(res) == 3)

model <- felm(
  bill_length_mm ~ bill_depth_mm | species,
  data = penguins[!is.na(penguins$body_mass_g), ],
  weights = penguins[!is.na(penguins$body_mass_g), ]$body_mass_g
)


res <- partialling_out(
  model,
  data = penguins[!is.na(penguins$body_mass_g), ],
  weights = penguins[!is.na(penguins$body_mass_g), ]$body_mass_g
) # nolint


expect_true(ncol(res) == 3)

## check that the names start with res_ in unweighted models ----

model <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)
res <- partialling_out(model, data = penguins)

expect_true(unique(startsWith(colnames(res), "res")))


model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)


expect_true(unique(startsWith(colnames(res), "res")))


model <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)


expect_true(unique(startsWith(colnames(res), "res")))


## test that all columns are numeric ----
model <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)
res <- partialling_out(model, data = penguins)

expect_true(unique(sapply(res, is.numeric)))

model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

expect_true(unique(sapply(res, is.numeric)))

model <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

expect_true(unique(sapply(res, is.numeric)))

## check that no NA value is returned ----
model <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)
res <- partialling_out(model, data = penguins)

navec <- apply(res, 1, function(x) any(is.na(x)))

expect_false(unique(navec))


model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

navec <- apply(res, 1, function(x) any(is.na(x)))

expect_false(unique(navec))

model <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

navec <- apply(res, 1, function(x) any(is.na(x)))

expect_false(unique(navec))

## check that no NaN value is returned ----

model <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)
res <- partialling_out(model, data = penguins)

navec <- apply(res, 1, function(x) any(is.nan(x)))

expect_false(unique(navec))

model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

navec <- apply(res, 1, function(x) any(is.nan(x)))

expect_false(unique(navec))

model <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

navec <- apply(res, 1, function(x) any(is.nan(x)))

expect_false(unique(navec))


## check that no inf value is returned ----
model <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)
res <- partialling_out(model, data = penguins)

navec <- apply(res, 1, function(x) any(is.infinite(x)))

expect_false(unique(navec))

model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

navec <- apply(res, 1, function(x) any(is.infinite(x)))

expect_false(unique(navec))

model <- felm(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)

navec <- apply(res, 1, function(x) any(is.infinite(x)))

expect_false(unique(navec))


# test plot ----

## lm no quantile ----
model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)
p1 <- function() plot_partial_residuals(res)

expect_snapshot_plot(p1, label = "lm no quantile")

tinytheme() # clean up theme


## lm quantile ----
model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)
p1 <- function() plot_partial_residuals(res, quantile = TRUE)

expect_snapshot_plot(p1, label = "lm quantile")

tinytheme() # clean up theme

## no lm no quantile ----
model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)
p1 <- function() plot_partial_residuals(res, add_lm = FALSE, quantile = FALSE)

expect_snapshot_plot(p1, label = "no lm no quantile")

tinytheme() # clean up theme

## no lm quantile ----
model <- feols(bill_length_mm ~ bill_depth_mm | species, data = penguins)
res <- partialling_out(model, data = penguins)
p1 <- function() plot_partial_residuals(res, add_lm = FALSE, quantile = TRUE)

expect_snapshot_plot(p1, label = "no lm quantile")

tinytheme() # clean up theme
