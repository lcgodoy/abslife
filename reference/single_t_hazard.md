# Hazard estimate for a single time-point.

Internal use.

## Usage

``` r
single_t_hazard(t, trunc_time, lifetime, event, censoring_indicator)
```

## Arguments

- t:

  A time point at which hazard estimates are sought.

- trunc_time:

  A numeric vector representing the observed left-truncated time.

- lifetime:

  A numeric vector representing the observed time to event.

- event:

  event indicator

- censoring_indicator:

  An indicator for censoring (1=censored, 0=not). Defaults to a vector
  of 0s if `NULL`. An observation is only treated as an event if
  status=1 AND censoring=0.

## Value

A vector containing the time to event, \\\hat{C}\_n\\, the number of
events, and the hazard estimate along with its standard error.

## Author

lcgodoy
