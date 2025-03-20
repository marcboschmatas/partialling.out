
<!-- README.md is generated from README.Rmd. Please edit that file -->

# partialling.out

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/partialling.out)](https://CRAN.R-project.org/package=partialling.out)
[![Codecov test
coverage](https://codecov.io/gh/marcboschmatas/partialling.out/graph/badge.svg)](https://app.codecov.io/gh/marcboschmatas/partialling.out)
[![R-CMD-check](https://github.com/marcboschmatas/partialling.out/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/marcboschmatas/partialling.out/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Partialling out is a package that allows to generate residualised
variables of already existing linear or fixed effects models. So far it
works with `lm`, `felm` (`lfe`package) and `feols` (`fixest` package)
following the Frisch-Waugh-Lovell theorem, as explained in Lovell
([2008](doi:10.3200/JECE.39.1.88-91)). Whereas this algorithm has
already been implemented in
\[`fwlplot`\]((<https://github.com/kylebutts/fwlplot>), this package
offers three new characteristics.

- It uses an already existing model instead of a formula.

- Works with `lm` and `felm` objects alongside `feols`.

- Returns a data.frame with residualised variables instead of a plot,
  thus offering more freedom of what to do with the results.

## Installation

You can install the development version of partialling.out from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("marcboschmatas/partialling.out")
```

## Examples

The workflow for `partialling.out` is rather simple: first, create a
linear or fixed effects model.

``` r
library(partialling.out)
library(tinytable)
library(tinyplot)
library(palmerpenguins)

model <- lm(bill_length_mm ~  bill_depth_mm + species, data = penguins)
summary(model)
#> 
#> Call:
#> lm(formula = bill_length_mm ~ bill_depth_mm + species, data = penguins)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -8.0300 -1.5828  0.0733  1.6925 10.0313 
#> 
#> Coefficients:
#>                  Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)       13.2164     2.2475    5.88 9.83e-09 ***
#> bill_depth_mm      1.3940     0.1220   11.43  < 2e-16 ***
#> speciesChinstrap   9.9390     0.3678   27.02  < 2e-16 ***
#> speciesGentoo     13.4033     0.5118   26.19  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.518 on 338 degrees of freedom
#>   (2 observations deleted due to missingness)
#> Multiple R-squared:  0.7892, Adjusted R-squared:  0.7874 
#> F-statistic: 421.9 on 3 and 338 DF,  p-value: < 2.2e-16
```

Using the `partialling_out` function, you can get the residualised
variable of interest (bill length) and of the first explanatory variable
(bill_length), i.e. it would return the residuals of the following two
regressions.

``` r

modely <- lm(bill_length_mm ~ species, data = penguins)

modelx <- lm(bill_depth_mm ~ species, data = penguins)
```

``` r
res <- partialling_out(model, data = penguins)

tt(head(res))
```

| res_bill_length_mm | res_bill_depth_mm |
|--------------------|-------------------|
| 0.3086093          | 0.3536424         |
| 0.7086093          | -0.9463576        |
| 1.5086093          | -0.3463576        |
| -2.0913907         | 0.9536424         |
| 0.5086093          | 2.2536424         |
| 0.1086093          | -0.5463576        |

Accordingly, the coefficient of `res_bill_depth_mm` in the model
`lm(res_bill_length_mm ~ res_bill_depth_mm)` will be the same of the
coefficient of `bill_depth_mm` in the original model.

``` r
resmodel <- lm(res_bill_length_mm ~ res_bill_depth_mm, data = res)

print(c(model$coefficients[2], resmodel$coefficients[2]))
#>     bill_depth_mm res_bill_depth_mm 
#>          1.394011          1.394011
```

## Acknowledgements

To the authors of the [fwlplot](https://github.com/kylebutts/fwlplot)
package, Kyle Butts and Grant McDermott, which has provided inspiration
and ideas for this project.

To my colleague Andreu Arenas-Jal for his insight and guiding.
