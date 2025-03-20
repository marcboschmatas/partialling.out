
# Placeholder with simple test
library(palmerpenguins)
library(fixest)
library(lfe)
library(tinytest)
library(partialling.out)
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



# expect equal coefficients


model <- lm(bill_length_mm ~  bill_depth_mm + species,
            data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm,
             data = res)

expect_equal(unname(signif(model$coefficients[2], 4)),
             unname(signif(resmod$coefficients[2], 4)))



model <- feols(bill_length_mm ~  bill_depth_mm | species,
               data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm,
             data = res)

expect_equal(unname(signif(model$coefficients[1], 4)),
             unname(signif(resmod$coefficients[2], 4)))



model <- felm(bill_length_mm ~  bill_depth_mm | species,
              data = penguins)
res <- partialling_out(model, data = penguins)

resmod <- lm(res_bill_length_mm ~ res_bill_depth_mm,
             data = res)

expect_equal(unname(signif(model$coefficients[1], 4)),
             unname(signif(resmod$coefficients[2], 4)))
