# Hazard rate

Estimate the non-parametric hazard rate for truncated and censored data.

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

  A numeric vector representing the observed time to event (or
  censoring).

- trunc_time:

  A numeric vector representing the observed left-truncation time. If
  `NULL` (the default), it is set to a vector of 0s (i.e., no
  left-truncation).

- censoring_indicator:

  An indicator for censoring (1 = censored, 0 = not). Defaults to a
  vector of 0s if `NULL`. An observation is only treated as an event (of
  a given type) if its censoring indicator is 0.

- event_type:

  An optional vector of event identifiers (e.g., `"Default"` and
  `"Prepayment"`) for competing risks (experimental). When it has more
  than one unique value, cause-specific hazards are estimated for each
  event type.

- support_lifetime_rv:

  A `vector` of time points at which to evaluate the hazard. If `NULL`
  (the default), it is calculated by
  [`calc_osup()`](http://lcgodoy.me/abslife/reference/calc_osup.md) as a
  sequence from `Delta + 1` to `omega` (that is, `max(lifetime)`).

- carry_hazard:

  A `logical` indicator on whether 0 hazard estimates should be replaced
  by the last non-zero estimate. Defaults to `FALSE`.

- ci_level:

  A number between 0 and 1 indicating the level of the confidence
  intervals.

## Value

An object of class `alife` (or `alife_multi`, when `event_type` has more
than one unique value), that is, a `data.frame` with the hazard
estimates, their standard errors, and asymptotic confidence intervals.
Importantly, in the output, the column called `se_log_hazard` represents
the standard error of the `hazard` rate at the `logit` scale.

## Details

Point estimate and asymptotic confidence intervals are calculated based
on Lautier et al. 2023,
[doi:10.1016/j.ecosta.2023.05.005](https://doi.org/10.1016/j.ecosta.2023.05.005)
; Lautier et al. 2025,
[doi:10.1214/25-AOAS2103](https://doi.org/10.1214/25-AOAS2103) .
