# Estimates the vector of hazard rates over the lifetime support (auxiliary)

This function calculates the estimated hazard rate, \\\hat{\lambda}\\,
for the entire observable lifetime. It is an auxiliary function for the
main user function,
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md).
(Please see
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
for more detailed user support.)

## Usage

``` r
.hazard_core(
  support,
  trunc_time,
  lifetime,
  censoring_indicator,
  event_indicator,
  carry_hazard
)
```

## Arguments

- support:

  A vector of all time points to estimate \\\hat{\lambda}\\.

- trunc_time:

  A numeric vector representing the observed left-truncation time. If
  `NULL` (the default), this becomes a zero vector (i.e., there is no
  left-truncation).

- lifetime:

  A numeric vector representing the minimum of the observed termination
  time or right-censoring time.

- censoring_indicator:

  An indicator to identify if an observation is right-censored (1) or
  complete (0). Defaults to a zero vector if `NULL` (i.e., there are no
  right-censored observations). An observation is only treated as an
  event (of a given type) if its right-censoring indicator is 0.

- event_indicator:

  A binary vector that is set to 1 for all observations with a
  terminating event type that matches the desired cause for the hazard
  rate estimate and 0 otherwise. When no competing events are present,
  this vector is set to 1 for all observations.

- carry_hazard:

  A `logical` indicator to determine whether or not zero hazard rate
  estimates should be replaced by the last non-zero hazard rate estimate
  (i.e., “carried-forward”). Defaults to `FALSE`.

## Value

a `data.frame` used within
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md).
