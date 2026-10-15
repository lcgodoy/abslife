# Hazard estimate for a single time-point

Internal use.

## Usage

``` r
single_t_hazard(t, trunc_time, lifetime, event, censoring_indicator)
```

## Arguments

- t:

  A time point at which hazard estimates are sought.

- trunc_time:

  A numeric vector representing the observed left-truncation time. If
  `NULL` (the default), it is set to a vector of 0s (i.e., no
  left-truncation).

- lifetime:

  A numeric vector representing the observed time to event (or
  censoring).

- event:

  event indicator

- censoring_indicator:

  An indicator for censoring (1 = censored, 0 = not). Defaults to a
  vector of 0s if `NULL`. An observation is only treated as an event (of
  a given type) if its censoring indicator is 0.

## Value

A named vector containing the time point (`lifetime`), the risk set
estimate \\\hat{U}(t)\\ (`risk_set`), the hazard estimate (`hazard`),
and the standard error of the hazard estimate at the logit scale
(`se_log_hazard`).

## Author

lcgodoy
