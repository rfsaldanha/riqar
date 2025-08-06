
<!-- README.md is generated from README.Rmd. Please edit that file -->

# riqar

<!-- badges: start -->

<!-- badges: end -->

Computes air pollutant IQAr index following [CONAMA/MMA
specification](https://www.gov.br/mma/pt-br/assuntos/meio-ambiente-urbano-recursos-hidricos-qualidade-ambiental/qualidade-do-ar/indice-de-qualidade-do-ar-iqar).

## Installation

You can install the development version of riqar with:

``` r
# install.packages("pak")
pak::pak("rfsaldanha/riqar")
```

## Example

You can use the package to compute the IQAr index for a specific air
pollutant.

``` r
library(riqar)

iqar_pol(x = 50, pol = "pm2.5")
#> [1] 79
```

Or compute the general IQAr index using several air pollutants
concentrations.

``` r
iqar(pm10 = 50, pm2.5 = 80, o3 = 60)
#> [1] 127
```

``` r
iqar(pm10 = 50, pm2.5 = 80, o3 = 60, label = TRUE)
#> [1] "N4 - Muito Ruim"
```
