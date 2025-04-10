
# Placeholder with simple test
library(palmerpenguins)
library(fixest)
library(lfe)
library(tinytest)
library(partialling.out)
library(tsibble)
library(units)

penguins <- penguins
set.seed(1234)

# general standards for tests

# nolint start
#' @srrstats {G5.0} *Tests use the `palmerpenguins` dataset which is available on CRAN or a purpose-made dataset created under a random seed.*
#' @srrstats {G5.1} *All datasets are available to the end user, either by being created within the tests file or available on CRAN*
#' @srrstats {G5.3} *The absence of NA, NaN, or inf values is properly tested.*
NULL
# NOLINT END


# Test for errors warnings etc ----

# nolint start
#' @srrstats {G5.2} *All possible error conditions are tested*
#' @srrstats {G5.2a} *All error or warning messages is uniques*
#' @srrstats {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*
NULL

# nolint end


## Expect error if no data is provided ----

expect_error({
              mod <- lm(bill_length_mm ~  bill_depth_mm + species,
                        data = penguins)
              partialling_out(mod)})


expect_error({
              mod <- feols(bill_length_mm ~  bill_depth_mm | species,
                           data = penguins)
              partialling_out(mod)})


expect_error({
  mod <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
  partialling_out(mod)})


## expect error if data is not a data.frame ----

expect_error({
              mod <- lm(bill_length_mm ~  bill_depth_mm + species,
                        data = penguins)
              partialling_out(mod, data = c(1,2,3))})


expect_error({
              mod <- feols(bill_length_mm ~  bill_depth_mm | species,
                           data = penguins)
              partialling_out(mod, data = c(1,2,3))})


expect_error({
  mod <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
  partialling_out(mod, data = c(1,2,3))})


## expect error if weights are non numeric ----

expect_error({
  mod <- lm(bill_length_mm ~  bill_depth_mm + species, data = penguins)
  partialling_out(mod, data = penguins, weights = rep("a", nrow(penguins)))})


expect_error({
  mod <- feols(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = rep("a", nrow(penguins)))})


expect_error({
  mod <- felm(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = rep("a", nrow(penguins)))})


## expect error if weights are of the wrong length ----

expect_error({
  mod <- lm(bill_length_mm ~  bill_depth_mm + species, data = penguins)
  partialling_out(mod, data = penguins, weights = c(1,2,3))})


expect_error({
  mod <- feols(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = c(1,2,3))})

expect_error({
  mod <- felm(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = c(1,2,3))})


## expect error if only one covariate or no fixed effects ----

expect_error({
  mod <- lm(bill_length_mm ~  bill_depth_mm, data = penguins)
  partialling_out(mod, data = penguins)})


expect_error({
  mod <- feols(bill_length_mm ~  bill_depth_mm, data = penguins)
  partialling_out(mod, data = penguins)})


expect_error({
  mod <- felm(bill_length_mm ~  bill_depth_mm, data = penguins)
  partialling_out(mod, data = penguins)})



## expect a warning if partial models are weighted but original model is not ----


expect_warning({
  mod <- lm(bill_length_mm ~  bill_depth_mm + species, data = penguins)
  partialling_out(mod, data = penguins, weights = penguins$body_mass_g)})



expect_warning({
  mod <- feols(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = penguins$body_mass_g)})


expect_warning({
  mod <- felm(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = penguins$body_mass_g)})



## expect a warning if original model is weighted but not partialling_out ----


expect_warning({
  mod <- lm(bill_length_mm ~  bill_depth_mm + species, data = penguins,
            weights = penguins$body_mass_g)
  partialling_out(mod, data = penguins)})



expect_warning({
  mod <- feols(bill_length_mm ~  bill_depth_mm | species, data = penguins,
               weights = penguins$body_mass_g)
  partialling_out(mod, data = penguins)})


expect_warning({
  mod <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins[!is.na(penguins$body_mass_g), ],
              weights = penguins[!is.na(penguins$body_mass_g), ]$body_mass_g)
  partialling_out(mod, data = penguins[!is.na(penguins$body_mass_g), ])})



## test that error will occur if weights are included but na are not omitted----




model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins,
            weights = penguins$body_mass_g)


expect_error(partialling_out(model, data = penguins,
                             weights = penguins$body_mass_g,
                             na.rm = FALSE))


model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins,
               weights = penguins$body_mass_g)


expect_error(partialling_out(model, data = penguins,
                             weights = penguins$body_mass_g,
                             na.rm = FALSE))


df <- penguins[!is.na(penguins$body_mass_g), ]

model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = df,
              weights = df$body_mass_g)


expect_error(partialling_out(model,
                             data = df,
                             weights = df$body_mass_g,
                             na.rm = FALSE))



## test that no error will return if "0" elements are included in felm ----


model <- felm(bill_length_mm ~ bill_depth_mm | species | 0 | species,
              data = penguins)

expect_silent(partialling_out(model,
                              data = penguins))

## test that column summaries will behave as expected ----

# nolint start
#' @srrstats {EA5.3} *summaries are tested to behave as expected*
NULL
# nolint end


model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins)
res <- partialling_out(model, data = penguins)

summaries <- lapply(res, summary)

sumnames <- unique(unlist(lapply(summaries, names)))

expect_equal(sumnames, c("Min.", "1st Qu.", "Median",
                         "Mean", "3rd Qu.", "Max."))

model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res <- partialling_out(model, data = penguins)


summaries <- lapply(res, summary)

sumnames <- unique(unlist(lapply(summaries, names)))

expect_equal(sumnames, c("Min.", "1st Qu.", "Median",
                         "Mean", "3rd Qu.", "Max."))



model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
res <- partialling_out(model, data = penguins)


summaries <- lapply(res, summary)

sumnames <- unique(unlist(lapply(summaries, names)))

expect_equal(sumnames, c("Min.", "1st Qu.", "Median",
                         "Mean", "3rd Qu.", "Max."))




## test that a non standard dataframe will be accepted---

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

model <- lm(dv ~ iv1 + iv2, data = df)

expect_silent(partialling_out(model, df))


model <- feols(dv ~ iv1 | iv2, data = df)

expect_silent(partialling_out(model, df))



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


model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm,
             data = res)

expect_equal(unname(signif(model$coefficients[2], 4)),
             unname(signif(resmod$coefficients[2], 4)))

### check that a different random seed will provide the same results ----

set.seed(5678)

model2 <- lm(bill_length_mm ~  bill_depth_mm + species,
             data = penguins)
res2 <- partialling_out(model2, data = penguins)

resmod2 <- lm(res_bill_length_mm ~ res_bill_depth_mm,
              data = res)

expect_equal(unname(signif(resmod$coefficients[2], 4)),
             unname(signif(resmod2$coefficients[2], 4)))


### check that random noise does not meaningfully change results ----


penguins$bill_length_mm2 <- penguins$bill_length_mm + .Machine$double.eps
penguins$bill_depth_mm2 <- penguins$bill_depth_mm + .Machine$double.eps


model3 <- lm(bill_length_mm2 ~  bill_depth_mm2 + species,
             data = penguins)
res <- partialling_out(model3, data = penguins)

resmod3 <- lm(res_bill_length_mm2 ~ res_bill_depth_mm2,
              data = res)

expect_equal(unname(signif(resmod$coefficients[2], 4)),
             unname(signif(resmod3$coefficients[2], 4)))



## feols ----
### test that fwl theorem is properly applied ----

set.seed(1234)

model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm,
             data = res)

expect_equal(unname(signif(model$coefficients[1], 4)),
             unname(signif(resmod$coefficients[2], 4)))



### check that a different random seed will provide the same results ----

set.seed(5678)

model2 <- feols(bill_length_mm ~  bill_depth_mm | species,
                data = penguins)
res2 <- partialling_out(model2, data = penguins)

resmod2 <- lm(res_bill_length_mm ~ res_bill_depth_mm,
              data = res)

expect_equal(unname(signif(resmod$coefficients[2], 4)),
             unname(signif(resmod2$coefficients[2], 4)))

penguins$bill_length_mm2 <- penguins$bill_length_mm + .Machine$double.eps
penguins$bill_depth_mm2 <- penguins$bill_depth_mm + .Machine$double.eps


### check that random noise does not meaningfully change results ----

model3 <- feols(bill_length_mm2 ~  bill_depth_mm2 + species,
             data = penguins)
res <- partialling_out(model3, data = penguins)

resmod3 <- lm(res_bill_length_mm2 ~ res_bill_depth_mm2,
              data = res)

expect_equal(unname(signif(resmod$coefficients[2], 4)),
             unname(signif(resmod3$coefficients[2], 4)))


## felm ----
### test that fwl theorem is properly applied ----

set.seed(1234)

model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm,
             data = res)

expect_equal(unname(signif(model$coefficients[1], 4)),
             unname(signif(resmod$coefficients[2], 4)))

### check that a different random seed will provide the same results ----

set.seed(5678)

model2 <- felm(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res2 <- partialling_out(model2, data = penguins)

resmod2 <- lm(res_bill_length_mm ~ res_bill_depth_mm,
              data = res)

expect_equal(unname(signif(resmod$coefficients[2], 4)),
             unname(signif(resmod2$coefficients[2], 4)))


### check that random noise does not meaningfully change results ----

model3 <- felm(bill_length_mm2 ~  bill_depth_mm2 + species,
                data = penguins)
res <- partialling_out(model3, data = penguins)

resmod3 <- lm(res_bill_length_mm2 ~ res_bill_depth_mm2,
              data = res)

expect_equal(unname(signif(resmod$coefficients[2], 4)),
             unname(signif(resmod3$coefficients[2], 4)))





# test for return object characteristics ----

# nolint start
#' @srrstats {EA6.0} *Return values from all functions are tested*
#' @srrstats {EA6.0a} *Classes and types of objects are tested*
#' @srrstats {EA6.0b} *number of columns is tested*
#' @srrstats {EA6.0c} *Column names (or equivalent) of tabular objects are tested*
#' @srrstats {EA6.0d} *Classes or types of all columns contained within `data.frame`-type tabular objects are tested*
NULL
# nolint end


## test that it returns a data.frame----



model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins)
res <- partialling_out(model, data = penguins)

expect_true("data.frame" %in% class(res))



model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res <- partialling_out(model, data = penguins)




expect_true("data.frame" %in% class(res))



model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
res <- partialling_out(model, data = penguins)




expect_true("data.frame" %in% class(res))


## test that it returns a data.frame of two columns if unweighted ----



model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins)
res <- partialling_out(model, data = penguins)

expect_true(ncol(res) == 2)



model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res <- partialling_out(model, data = penguins)




expect_true(ncol(res) == 2)



model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
res <- partialling_out(model, data = penguins)




expect_true(ncol(res) == 2)


## check that it returns three columns if weighted ----


model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins,
            weights = penguins$body_mass_g)
res <- partialling_out(model, data = penguins,
                       weights = penguins$body_mass_g)

expect_true(ncol(res) == 3)



model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins,
               weights = penguins$body_mass_g)
res <- partialling_out(model, data = penguins,
                       weights = penguins$body_mass_g)




expect_true(ncol(res) == 3)



model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins[!is.na(penguins$body_mass_g), ],
              weights = penguins[!is.na(penguins$body_mass_g), ]$body_mass_g)


res <- partialling_out(model, data = penguins[!is.na(penguins$body_mass_g), ],
                       weights = penguins[!is.na(penguins$body_mass_g), ]$body_mass_g) # nolint




expect_true(ncol(res) == 3)

## check that the names start with res_ in unweighted models ----


model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins)
res <- partialling_out(model, data = penguins)

expect_true(unique(startsWith(colnames(res), "res")))



model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res <- partialling_out(model, data = penguins)


expect_true(unique(startsWith(colnames(res), "res")))



model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
res <- partialling_out(model, data = penguins)


expect_true(unique(startsWith(colnames(res), "res")))


## test that all columns are numeric ----


model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins)
res <- partialling_out(model, data = penguins)

expect_true(unique(sapply(res, is.numeric)))



model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res <- partialling_out(model, data = penguins)


expect_true(unique(sapply(res, is.numeric)))



model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
res <- partialling_out(model, data = penguins)


expect_true(unique(sapply(res, is.numeric)))




## check that no NA value is returned ----


model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins)
res <- partialling_out(model, data = penguins)


navec <- apply(res, 1, function(x) any(is.na(x)))


expect_false(unique(navec))



model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res <- partialling_out(model, data = penguins)


navec <- apply(res, 1, function(x) any(is.na(x)))


expect_false(unique(navec))

model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
res <- partialling_out(model, data = penguins)


navec <- apply(res, 1, function(x) any(is.na(x)))


expect_false(unique(navec))




## check that no NaN value is returned ----


model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins)
res <- partialling_out(model, data = penguins)


navec <- apply(res, 1, function(x) any(is.nan(x)))


expect_false(unique(navec))



model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res <- partialling_out(model, data = penguins)


navec <- apply(res, 1, function(x) any(is.nan(x)))


expect_false(unique(navec))


model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
res <- partialling_out(model, data = penguins)


navec <- apply(res, 1, function(x) any(is.nan(x)))


expect_false(unique(navec))


## check that no inf value is returned ----


model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins)
res <- partialling_out(model, data = penguins)


navec <- apply(res, 1, function(x) any(is.infinite(x)))


expect_false(unique(navec))



model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res <- partialling_out(model, data = penguins)


navec <- apply(res, 1, function(x) any(is.infinite(x)))


expect_false(unique(navec))


model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
res <- partialling_out(model, data = penguins)


navec <- apply(res, 1, function(x) any(is.infinite(x)))


expect_false(unique(navec))


