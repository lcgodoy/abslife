# Estimating the discrete time, hazard rate subject to left-truncation

This function calculates the estimated hazard rate, \\\hat{\lambda}\\,
for the entire vector of observable support. It is the discrete time,
hazard rate estimator adjusted for random left-truncation,
right-censoring, and competing risks (Lautier et al., 2026a).

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

  A numeric vector representing the minimum of the observed termination
  time or right-censoring time.

- trunc_time:

  A numeric vector representing the observed left-truncation time. If
  `NULL` (the default), this becomes a zero vector (i.e., there is no
  left-truncation).

- censoring_indicator:

  An indicator to identify if an observation is right-censored (1) or
  complete (0). Defaults to a zero vector if `NULL` (i.e., there are no
  right-censored observations). An observation is only treated as an
  event (of a given type) if its right-censoring indicator is 0.

- event_type:

  An optional vector of event identifiers (e.g., `"Default"` and
  `"Prepayment"`) for the case of competing risks. When this field has
  more than one unique value, cause-specific hazards are estimated for
  each unique event type.

- support_lifetime_rv:

  A `vector` of time points at which to evaluate the hazard. If `NULL`
  (the default), it is calculated by
  [`calc_osup()`](http://lcgodoy.me/abslife/reference/calc_osup.md) as a
  sequence from \\\Delta + 1\\ to \\\xi\\ (that is, `max(lifetime)`).

- carry_hazard:

  A `logical` indicator to determine whether or not zero hazard rate
  estimates should be replaced by the last non-zero hazard rate estimate
  (i.e., “carried-forward”). Defaults to `FALSE`.

- ci_level:

  A number between 0 and 1 indicating the desired confidence interval
  width. Defaults to 0.95.

## Value

An object of class `alife` (or `alife_multi`, when `event_type` has more
than one unique value). These objects are each a `data.frame` with the
time point, hazard estimate, standard error, and asymptotic confidence
interval for all time points in the support. We emphasize that the
column called `se_log_hazard` represents the standard error of the
`hazard` rate on the logit scale.

## Details

Returns the vector of hazard rate estimates from Lautier et al. (2026a,
Theorem 2.1) with asymptotic confidence intervals from Lautier et al.
(2026a, Lemma 1). When there are no competing risks, this function
returns the equivalent estimates and asymptotic confidence intervals
from Lautier et al. (2023). When there are no competing risks and no
right-censoring, the estimates and asymptotic confidence intervals
follow from Lautier et al. (2026b).

## References

Lautier, J. P., Pozdnyakov, V., Yan, J. (2023). Pricing time-to-event
contingent cash-flows: A discrete-time survival analysis approach.
*Insurance: Mathematics and Economics*, 110, 53-71.
<https://doi.org/10.1016/j.insmatheco.2023.02.003>

Lautier, J. P., Pozdnyakov, V., Yan, J. (2026a). On the convergence of
credit risk in current consumer automobile loans. *Journal of the Royal
Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
<https://doi.org/10.1093/jrsssa/qnae137>

Lautier, J. P., Pozdnyakov, V., Yan, J. (2026b). Estimating a discrete
distribution subject to random left-truncation with an application to
structured finance. *Econometrics and Statistics*, 37, 174-198.
<https://doi.org/10.1016/j.ecosta.2023.05.005>
