# Solve for Risk-Adjusted Internal Rate of Return (IRR)

Finds the monthly discount rate at which the APV of the loan (see
[`calculate_apv()`](http://lcgodoy.me/abslife/reference/calculate_apv.md))
matches `abs0_bal`. The rate is found with
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) over monthly
rates between -0.9999 and 1; an error is thrown if no rate in this range
matches `abs0_bal`. If
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) does not
reach a tolerance of `1e-10` within 1000 iterations,
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) gives a
warning and the `converged` column is `FALSE`.

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

  Current loan balance (to match the APV against).

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

  Internal loan APY for amortization. Defaults to 0.15.

- type:

  A character string specifying the extrapolation type passed to
  [`extend_hazard()`](http://lcgodoy.me/abslife/reference/extend_hazard.md):
  either `"constant"` (default), `"geometric"` or `"linear"`.

## Value

An object of class `irr`: a one-row `data.frame` with the risk-adjusted
monthly rate (`monthly_rate`), the annualized rate (`annualized_rate`),
the squared difference between `abs0_bal` and the APV at the solution
(`objective`), and a flag that tells if the root finder converged
(`converged`). [`rbind()`](https://rdrr.io/r/base/cbind.html) stacks
several `irr` objects into a `data.frame`; named arguments become row
names.

## Details

`x` must contain exactly two event types, and the default event must be
labeled either `"Default"` or `"1"`; the other event type is treated as
prepayment. If the observed lifetime support does not reach `orig_term`,
the hazards are extrapolated up to `orig_term` with
[`extend_hazard()`](http://lcgodoy.me/abslife/reference/extend_hazard.md)
(with a warning). After that, hazard estimates must be available for
every lifetime from `cur_age + 1` to `orig_term`; otherwise, an error is
thrown.
