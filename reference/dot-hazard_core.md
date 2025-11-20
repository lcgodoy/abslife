# Auxiliary function for `estimate_hazard`

Auxiliary function for `estimate_hazard`

## Usage

``` r
.hazard_core(
  support,
  trunc_time,
  time_to_event,
  censoring,
  event_indicator,
  carry_hazard
)
```

## Arguments

- support:

  where to calculate the hazards

- trunc_time:

  A numeric vector representing the observed left-truncated time.

- time_to_event:

  A numeric vector representing the observed time to event.

- censoring:

  An indicator for censoring (1=censored, 0=not). Defaults to a vector
  of 0s if `NULL`. An observation is only treated as an event if
  status=1 AND censoring=0.

- event_indicator:

  legacy.

- carry_hazard:

  A `boolean` indicator on whether 0 hazard estimates should be replaced
  by the last non-zero estimate. Defaults to `FALSE`

## Value

a `data.frame`

## Author

lcgodoy
