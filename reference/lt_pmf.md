# Core for left-truncation PMF computation

Internal use

## Usage

``` r
.lt_core(lifetime, denom, trunc_time, rc = TRUE)

lt_pmf(x, trunc_time, rc = TRUE)
```

## Arguments

- lifetime:

  A numeric vector representing the observed time to event.

- denom:

  A `numeric` vector representing the denominator for the calculation of
  the pmf

- trunc_time:

  A numeric vector representing the observed left-truncated time.

- rc:

  a `numeric` boolean indicating whether right-censoring is to be
  considered.

- x:

  is the output of an `estimate_hazard` call.

## Value

A `numeric` vector of estimates of the PMF for the left-truncation
random variable.

## Author

lcgodoy
