# Hazard rate

Estimate the non-parametric hazard rate for truncated and censored data

## Usage

``` r
estimate_hazard(
  lifetime,
  trunc_time = NULL,
  censoring_indicator = NULL,
  event_type = NULL,
  support_lifetime_rv = NULL,
  carry_hazard = FALSE,
  ci_level = 0.95
)
```

## Arguments

- lifetime:

  A numeric vector representing the observed time to event.

- trunc_time:

  A numeric vector representing the observed left-truncated time.

- censoring_indicator:

  An indicator for censoring (1=censored, 0=not). Defaults to a vector
  of 0s if `NULL`. An observation is only treated as an event if
  status=1 AND censoring=0.

- event_type:

  a vector of "events identifies" (experimental)

- support_lifetime_rv:

  A `vector` of time points at which to evaluate the hazard. If `NULL`
  (the default), it is calculated for a sequence from `Delta + 1` to
  `omega` (that is, `max(lifetime)`).

- carry_hazard:

  A `boolean` indicator on whether 0 hazard estimates should be replaced
  by the last non-zero estimate. Defaults to `FALSE`

- ci_level:

  A number between 0 and 1 indicating the level of the confidence
  intervals.

## Value

A `data.frame` with the hazard estimate their standard errors and
asymptotic confidence intervals.

## Details

Point estimate and asymptotic confidence intervals are calculated based
on (We can also include some brief notation/definitions here)
