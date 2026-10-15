# Empirical risk set

This function calculates \\\hat{U}\\, which is the denominator of the
cause-specific hazard rate estimator from Lautier et al. (2026a). This
quantity adjusts \\\hat{f}\\ (see
[`f_hat()`](http://lcgodoy.me/abslife/reference/f_hat.md)) for the
inherent survival bias due to left-truncation. In survival analysis,
\\\hat{U}\\ is commonly referred to as the *risk set*.

## Usage

``` r
u_hat(t, lifetime, trunc_time)
```

## Arguments

- t:

  The desired estimation time point.

- lifetime:

  A numeric vector representing the minimum of the observed termination
  time or right-censoring time.

- trunc_time:

  A numeric vector representing the observed left-truncation time. If
  `NULL` (the default), this becomes a zero vector (i.e., there is no
  left-truncation).

## Value

a scalar probability between 0 and 1

## Note

The \\\hat{U}\\ estimator simplifies to its equivalent in Lautier et al.
(2026b) if there is no right-censoring present in the data.

## References

Lautier, J. P., Pozdnyakov, V., Yan, J. (2026a). On the convergence of
credit risk in current consumer automobile loans. *Journal of the Royal
Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
<https://doi.org/10.1093/jrsssa/qnae137>

Lautier, J. P., Pozdnyakov, V., Yan, J. (2026b). Estimating a discrete
distribution subject to random left-truncation with an application to
structured finance. *Econometrics and Statistics*, 37, 174-198.
<https://doi.org/10.1016/j.ecosta.2023.05.005>
