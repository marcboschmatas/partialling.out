
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
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/703_status.svg)](https://github.com/ropensci/software-review/issues/703)
<!-- badges: end -->

Partialling out is a package that allows to generate residualised
variables of already existing linear or fixed effects models. So far it
works with `lm`, `felm` (`lfe`package) and `feols` (`fixest` package)
for applications of the Frisch-Waugh-Lovell theorem, as explained in
Lovell ([2008](doi:10.3200/JECE.39.1.88-91)). Whereas this algorithm has
already been implemented in
[`fwlplot`](https://github.com/kylebutts/fwlplot), this package offers
three new characteristics.

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

model <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)
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
(bill\_length), i.e. it would return the residuals of the following two
regressions.

``` r

modely <- lm(bill_length_mm ~ species, data = penguins)

modelx <- lm(bill_depth_mm ~ species, data = penguins)
```

``` r
res <- partialling_out(model, data = penguins)

tt(head(res)) |>
  format_tt(digits = 2) |>
  style_tt(align = "c")
```

| res\_bill\_length\_mm | res\_bill\_depth\_mm |
| --------------------- | -------------------- |
| 0.31                  | 0.35                 |
| 0.71                  | \-0.95               |
| 1.51                  | \-0.35               |
| \-2.09                | 0.95                 |
| 0.51                  | 2.25                 |
| 0.11                  | \-0.55               |

## Checking the results

The Frisch-Waugh-Lovell theorem states that for a linear model   
![&#10;Y = X\_0 \\beta\_1 + X\_2 \\beta\_2 +
u&#10;](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0AY%20%3D%20X_0%20%5Cbeta_1%20%2B%20X_2%20%5Cbeta_2%20%2B%20u%0A
"
Y = X_0 \\beta_1 + X_2 \\beta_2 + u
")  

The coefficient
![\\beta\_2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_2
"\\beta_2") will be equivalent to that in the regression

  
![&#10;M\_{X\_1} Y = M\_{X\_1}X\_2\\beta\_2 +
M\_{X\_1}u&#10;](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0AM_%7BX_1%7D%20Y%20%3D%20M_%7BX_1%7DX_2%5Cbeta_2%20%2B%20M_%7BX_1%7Du%0A
"
M_{X_1} Y = M_{X_1}X_2\\beta_2 + M_{X_1}u
")  

Where
![M\_{X\_1}Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;M_%7BX_1%7DY
"M_{X_1}Y") are the residuals of the model

  
![&#10;Y = X\_1 \\beta\_1 +
u&#10;](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0AY%20%3D%20X_1%20%5Cbeta_1%20%2B%20u%0A
"
Y = X_1 \\beta_1 + u
")  

And
![M\_{X\_1}X\_2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;M_%7BX_1%7DX_2
"M_{X_1}X_2") those of

  
![&#10;X\_2 = X\_1 \\beta\_1 +
u&#10;](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0AX_2%20%3D%20X_1%20%5Cbeta_1%20%2B%20u%0A
"
X_2 = X_1 \\beta_1 + u
")  

Accordingly, the coefficient of `res_bill_depth_mm` in the model
`lm(res_bill_length_mm ~ res_bill_depth_mm)` will be the same of the
coefficient of `bill_depth_mm` in the original model.

``` r
resmodel <- lm(res_bill_length_mm ~ res_bill_depth_mm, data = res)

print(c(model$coefficients[2], resmodel$coefficients[2]))
#>     bill_depth_mm res_bill_depth_mm 
#>          1.394011          1.394011
```

## Contributing

Contributing instructions can be found
[here](https://github.com/marcboschmatas/partialling.out/blob/main/.github/contributing.md)

## Acknowledgements

To the authors of the [fwlplot](https://github.com/kylebutts/fwlplot)
package, Kyle Butts and Grant McDermott, which has provided inspiration
and ideas for this project.

To my colleague Andreu Arenas-Jal for his insight and guiding.
