# Auxiliary function for `estimate_hazard`

Auxiliary function for `estimate_hazard`

## Usage

``` r
check_censored(lifetime, censoring_indicator, support_lifetime_rv)
```

## Arguments

- lifetime:

  A numeric vector representing the observed time to event.

- censoring_indicator:

  An indicator for censoring (1=censored, 0=not). Defaults to a vector
  of 0s if `NULL`. An observation is only treated as an event if
  status=1 AND censoring=0.

- support_lifetime_rv:

  A `vector` of time points at which to evaluate the hazard. If `NULL`
  (the default), it is calculated for a sequence from `Delta + 1` to
  `omega` (that is, `max(lifetime)`).

## Value

Nothing

## Author

lcgodoy
