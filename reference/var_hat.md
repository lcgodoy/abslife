# Asymptotic variance for log-transformed confidence intervals

This function calculates the estimated asymptotic variance of the
log-transformed hazard rate estimates. It is used to construct the
asymptotic confidence intervals for the hazard rate estimates. The
log-transformation ensures the estimated confidence intervals are
bounded between 0 and 1 (Lautier et al., 2026a).

## Usage

``` r
var_hat(lambda, risk_set, fh, n)
```

## Arguments

- lambda:

  hazard rate (currently unused).

- risk_set:

  The estimated risk set, \\\hat{U}\\ (see
  [`u_hat()`](http://lcgodoy.me/abslife/reference/u_hat.md)).

- fh:

  The estimated, unadjusted event probabilities, \\\hat{f}\\ (see
  [`f_hat()`](http://lcgodoy.me/abslife/reference/f_hat.md)).

- n:

  The sample size.

## Value

a positive scalar

## Note

The asymptotic, log-transformed variance estimator simplifies to its
equivalent in Lautier et al. (2023) if there is only one event type
(i.e., no competing risks). If there is both only one event type and no
right-censoring, then the asymptotic, log-transformed variance estimator
further simplifies to its equivalent in Lautier et al. (2026b).

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
