# \\\hat{U}(x)\\

\\\hat{U}(x)\\

## Usage

``` r
u_hat(t, lifetime, trunc_time)
```

## Arguments

- t:

  A time point at which hazard estimates are sought.

- lifetime:

  A numeric vector representing the observed time to event (or
  censoring).

- trunc_time:

  A numeric vector representing the observed left-truncation time. If
  `NULL` (the default), it is set to a vector of 0s (i.e., no
  left-truncation).

## Value

a scalar

## Author

lcgodoy
