# Empirical frequencies, unadjusted

This function calculates \\\hat{f}\\, which is the numerator of the
cause-specific hazard rate estimator from Lautier et al. (2026a).

## Usage

``` r
f_hat(t, lifetime, event, censoring_indicator)
```

## Arguments

- t:

  The desired estimation time point.

- lifetime:

  A numeric vector representing the minimum of the observed termination
  time or right-censoring time.

- event:

  An event indicator for each competing risk.

- censoring_indicator:

  An indicator to identify if an observation is right-censored (1) or
  complete (0). Defaults to a zero vector if `NULL` (i.e., there are no
  right-censored observations). An observation is only treated as an
  event (of a given type) if its right-censoring indicator is 0.

## Value

a scalar probability between 0 and 1

## Note

The \\\hat{f}\\ estimator simplifies to its equivalent in Lautier et al.
(2023) if there is only one event type (i.e., no competing risks). If
there is both only one event type and no right-censoring, then the
\\\hat{f}\\ estimator further simplifies to its equivalent in Lautier et
al. (2026b).

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
