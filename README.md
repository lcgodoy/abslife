
<!-- README.md is generated from README.Rmd. Please edit that file -->

# abslife

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/abslife)](https://CRAN.R-project.org/package=abslife)
<!-- badges: end -->

> Under constructon!

The goal of abslife is to …

## Installation

You can install the development version of abslife like so:

``` r
remotes::install_github("lcgodoy/abslife")
```

## Example

``` r
library(abslife)
#> Welcome to the abslife package!
#> WARNING: Under construction.
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
data(aloans)
summary(aloans)
#>    risk_cat               Z               Y               C         
#>  Length:58118       Min.   : 3.00   Min.   : 2.00   Min.   :0.0000  
#>  Class :character   1st Qu.:17.00   1st Qu.: 3.00   1st Qu.:0.0000  
#>  Mode  :character   Median :32.00   Median : 4.00   Median :0.0000  
#>                     Mean   :32.82   Mean   : 4.44   Mean   :0.2348  
#>                     3rd Qu.:53.00   3rd Qu.: 5.00   3rd Qu.:0.0000  
#>                     Max.   :70.00   Max.   :19.00   Max.   :1.0000  
#>        D                R              bond          
#>  Min.   :0.0000   Min.   :0.0000   Length:58118      
#>  1st Qu.:0.0000   1st Qu.:0.0000   Class :character  
#>  Median :0.0000   Median :0.0000   Mode  :character  
#>  Mean   :0.3656   Mean   :0.3996                     
#>  3rd Qu.:1.0000   3rd Qu.:1.0000                     
#>  Max.   :1.0000   Max.   :1.0000
```
