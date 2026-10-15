# PMF of the left-truncation random variable

Estimates the probability mass function of the left-truncation random
variable. `.lt_core` is the internal workhorse.

## Usage

``` r
.lt_core(lifetime, denom, trunc_time, rc = TRUE)

lt_pmf(x, trunc_time, rc = TRUE)
```

## Arguments

- lifetime:

  A numeric vector representing the minimum of the observed termination
  time or right-censoring time.

- denom:

  A `numeric` vector representing the denominator for the calculation of
  the pmf

- trunc_time:

  A numeric vector representing the observed left-truncation time. If
  `NULL` (the default), this becomes a zero vector (i.e., there is no
  left-truncation).

- rc:

  a `logical` indicating whether right-censoring is to be considered.

- x:

  the output of an
  [`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
  call (an `alife` object) or of a
  [`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md) call
  (an `acdf` object). Competing risks are not supported.

## Value

A `numeric` vector of estimates of the PMF for the left-truncation
random variable.

## Author

lcgodoy
