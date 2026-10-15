# Check for censoring at the end of the support (auxiliary function for `estimate_hazard`)

Throws a warning when there are censored observations at the maximum of
`support_lifetime_rv`.

## Usage

``` r
check_censored(lifetime, censoring_indicator, support_lifetime_rv)
```

## Arguments

- lifetime:

  A numeric vector representing the observed time to event (or
  censoring).

- censoring_indicator:

  An indicator for censoring (1 = censored, 0 = not). Defaults to a
  vector of 0s if `NULL`. An observation is only treated as an event (of
  a given type) if its censoring indicator is 0.

- support_lifetime_rv:

  A `vector` of time points at which to evaluate the hazard. If `NULL`
  (the default), it is calculated by
  [`calc_osup()`](http://lcgodoy.me/abslife/reference/calc_osup.md) as a
  sequence from `Delta + 1` to `omega` (that is, `max(lifetime)`).

## Value

`NULL`, invisibly. Called for its side effect.

## Author

lcgodoy
