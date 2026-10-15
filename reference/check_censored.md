# Boundary point verification

The support conditions of Lautier et al. (2026) require that any
observations at the maximum observed lifetime time point are complete
(i.e., there are no right-censored observations at the maximum of
`support_lifetime_rv`). If this is not true, then a user will need to
decide on how to extrapolate beyond the final observed time point. As
such, this function triggers a warning message for users within
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md).
Hence, it is an auxiliary function for use within
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md).

## Usage

``` r
check_censored(lifetime, censoring_indicator, support_lifetime_rv)
```

## Arguments

- lifetime:

  A numeric vector representing the minimum of the observed termination
  time or right-censoring time.

- censoring_indicator:

  An indicator to identify if an observation is right-censored (1) or
  complete (0). Defaults to a zero vector if `NULL` (i.e., there are no
  right-censored observations). An observation is only treated as an
  event (of a given type) if its right-censoring indicator is 0.

- support_lifetime_rv:

  A `vector` of time points at which to evaluate the hazard. If `NULL`
  (the default), it is calculated by
  [`calc_osup()`](http://lcgodoy.me/abslife/reference/calc_osup.md) as a
  sequence from \\\Delta + 1\\ to \\\xi\\ (that is, `max(lifetime)`).

## Value

`NULL`. This is an invisible function called for its connection to a
potential warning message within
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md).

## References

Lautier, J. P., Pozdnyakov, V., Yan, J. (2026). On the convergence of
credit risk in current consumer automobile loans. *Journal of the Royal
Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
<https://doi.org/10.1093/jrsssa/qnae137>
