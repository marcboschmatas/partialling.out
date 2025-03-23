
# Placeholder with simple test
library(palmerpenguins)
library(fixest)
library(lfe)
library(tinytest)
library(partialling.out)

set.seed(1234)
# Expect error if no data is provided

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


# expect error if data is not a data.frame

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


# expect error if weights are non numeric

expect_error({
  mod <- lm(bill_length_mm ~  bill_depth_mm + species, data = penguins)
  partialling_out(mod, data = penguins, weights = rep("a", nrow(penguins)))})


expect_error({
  mod <- feols(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = rep("a", nrow(penguins)))})


expect_error({
  mod <- felm(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = rep("a", nrow(penguins)))})


# expect error if weights are of the wrong length

expect_error({
  mod <- lm(bill_length_mm ~  bill_depth_mm + species, data = penguins)
  partialling_out(mod, data = penguins, weights = c(1,2,3))})


expect_error({
  mod <- feols(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = c(1,2,3))})

expect_error({
  mod <- felm(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = c(1,2,3))})


# expect error if only one covariate or no fixed effects

expect_error({
  mod <- lm(bill_length_mm ~  bill_depth_mm, data = penguins)
  partialling_out(mod, data = penguins)})


expect_error({
  mod <- feols(bill_length_mm ~  bill_depth_mm, data = penguins)
  partialling_out(mod, data = penguins)})


expect_error({
  mod <- felm(bill_length_mm ~  bill_depth_mm, data = penguins)
  partialling_out(mod, data = penguins)})



# expect a warning if partial models are weighted but original model is not


expect_warning({
  mod <- lm(bill_length_mm ~  bill_depth_mm + species, data = penguins)
  partialling_out(mod, data = penguins, weights = penguins$body_mass_g)})



expect_warning({
  mod <- feols(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = penguins$body_mass_g)})


expect_warning({
  mod <- felm(bill_length_mm ~  bill_depth_mm | species, data = penguins)
  partialling_out(mod, data = penguins, weights = penguins$body_mass_g)})



# expect a warning if the original model is weighted but partialling out is not


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

# test that fwl theorem is properly applied


model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm,
             data = res)

expect_equal(unname(signif(model$coefficients[2], 4)),
             unname(signif(resmod$coefficients[2], 4)))

# check that a different random seed will provide the same results

set.seed(5678)

model2 <- lm(bill_length_mm ~  bill_depth_mm + species,
             data = penguins)
res2 <- partialling_out(model, data = penguins)

resmod2 <- lm(res_bill_length_mm ~ res_bill_depth_mm,
              data = res)

expect_equal(unname(signif(resmod$coefficients[2], 4)),
             unname(signif(resmod2$coefficients[2], 4)))

# test that fwl theorem is properly applied

set.seed(1234)

model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm,
             data = res)

expect_equal(unname(signif(model$coefficients[1], 4)),
             unname(signif(resmod$coefficients[2], 4)))



# check that a different random seed will provide the same results

set.seed(5678)

model2 <- feols(bill_length_mm ~  bill_depth_mm | species,
                data = penguins)
res2 <- partialling_out(model, data = penguins)

resmod2 <- lm(res_bill_length_mm ~ res_bill_depth_mm,
              data = res)

expect_equal(unname(signif(resmod$coefficients[2], 4)),
             unname(signif(resmod2$coefficients[2], 4)))


# test that fwl theorem is properly applied

set.seed(1234)

model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm,
             data = res)

expect_equal(unname(signif(model$coefficients[1], 4)),
             unname(signif(resmod$coefficients[2], 4)))

# check that a different random seed will provide the same results

set.seed(5678)

model2 <- felm(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res2 <- partialling_out(model, data = penguins)

resmod2 <- lm(res_bill_length_mm ~ res_bill_depth_mm,
              data = res)

expect_equal(unname(signif(resmod$coefficients[2], 4)),
             unname(signif(resmod2$coefficients[2], 4)))



# check that no NA value is returned


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




# check that no NaN value is returned


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


# check that no inf value is returned


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



# test that it returns a data.frame








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


# test that it returns a data.frame of two columns



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

# check that the names start with res_


# test that it returns a data.frame of two columns



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


# test that the two columns are numeric




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


