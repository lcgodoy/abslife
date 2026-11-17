# Solves for the risk-adjusted internal rate of return (IRR)

Finds the monthly discount rate such that the actuarial present value
(APV) of the loan (see
[`calculate_apv()`](http://lcgodoy.me/abslife/reference/calculate_apv.md))
matches `abs0_bal`. For theoretical details, see Lautier et al. (2026).
Because this calculation must solve a polynomial of high-degree, the
rate is found by using
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) and searching
over monthly effective rates between -0.9999 and 1. If no solution can
be found, an `error` will be thrown. If
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) does not
reach a tolerance of `1e-10` within 1,000 iterations,
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) will produce
a warning and the `converged` column of the resulting `irr` object will
be `FALSE`.

## Usage

``` r
solve_irr(
  x,
  abs0_bal,
  cur_age,
  orig_term,
  orig_loan_amt,
  mon_pmt,
  recov_curve,
  orig_apy = 0.15,
  type = "constant"
)
```

## Arguments

- x:

  An object of class `alife_multi`. Typically the output of the
  `estimate_hazard` function with two event types.

- abs0_bal:

  Current loan balance, interpreted as the price of the loan (i.e., we
  find the risk-adjusted rate such that the current loan balance matches
  the calculated APV).

- cur_age:

  Current age of the loan in months.

- orig_term:

  Original loan term in months.

- orig_loan_amt:

  Original loan amount.

- mon_pmt:

  Monthly payment amount.

- recov_curve:

  Recovery curve `data.frame` with columns `month` and `recovery` (the
  recovery rate as a proportion of `orig_loan_amt`). It must contain
  every month from `cur_age` to `orig_term - 1`.

- orig_apy:

  Internal loan annual percentage yield (APY) for amortization. Defaults
  to 0.15.

- type:

  A character string specifying the extrapolation type passed to
  [`extend_hazard()`](http://lcgodoy.me/abslife/reference/extend_hazard.md):
  `constant` (default), `geometric` or `linear`.

## Value

An object of class `irr`: a one-row `data.frame` with the risk-adjusted
monthly rate (`monthly_rate`), the annualized rate (`annualized_rate`),
the squared difference between `abs0_bal` and the APV at the solution
(`objective`), and a flag indicating if the root finder converged
(`converged`). Note that [`rbind()`](https://rdrr.io/r/base/cbind.html)
stacks several `irr` objects into a `data.frame`, and its named
arguments become its row names.

## Details

`x` must contain exactly two event types, and the default event must be
labeled either `Default` or `1`; the other event type will be treated as
a prepayment. If the observed lifetime support does not reach
`orig_term`, the hazard rates are extrapolated up to `orig_term` with
[`extend_hazard()`](http://lcgodoy.me/abslife/reference/extend_hazard.md).
If this occurs, a `warning` message will be thrown. Hazard rate
estimates must be available for every lifetime from `cur_age + 1` to
`orig_term`; otherwise, an `error` will be thrown.

## References

Lautier, J. P., Pozdnyakov, V., Yan, J. (2026). On the convergence of
credit risk in current consumer automobile loans. *Journal of the Royal
Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
<https://doi.org/10.1093/jrsssa/qnae137>
