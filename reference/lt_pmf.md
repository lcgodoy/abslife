# Returns the estimated density of the left-truncation random variable

Estimates the probability mass function of the left-truncation random
variable from an `alife` or `acdf` object.

## Usage

``` r
lt_pmf(x, trunc_time, rc = TRUE)
```

## Arguments

- x:

  An
  [`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
  (i.e., `alife`) object or a
  [`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md) (i.e.,
  `acdf`) object.

- trunc_time:

  A numeric vector representing the observed left-truncation time. If
  `NULL` (the default), this becomes a zero vector (i.e., there is no
  left-truncation).

- rc:

  A `logical` indicating whether right-censoring is to be considered.

## Value

A `numeric` vector of estimates of the PMF for the left-truncation
random variable.
