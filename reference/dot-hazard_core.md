# Hazard estimates over a support (auxiliary function for `estimate_hazard`)

Hazard estimates over a support (auxiliary function for
`estimate_hazard`)

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

  where to calculate the hazards

- trunc_time:

  A numeric vector representing the observed left-truncation time. If
  `NULL` (the default), it is set to a vector of 0s (i.e., no
  left-truncation).

- lifetime:

  A numeric vector representing the observed time to event (or
  censoring).

- censoring_indicator:

  An indicator for censoring (1 = censored, 0 = not). Defaults to a
  vector of 0s if `NULL`. An observation is only treated as an event (of
  a given type) if its censoring indicator is 0.

- event_indicator:

  A binary vector flagging the observations whose event is of the type
  being estimated (all ones when there are no competing risks).

- carry_hazard:

  A `logical` indicator on whether 0 hazard estimates should be replaced
  by the last non-zero estimate. Defaults to `FALSE`.

## Value

a `data.frame`

## Author

lcgodoy
