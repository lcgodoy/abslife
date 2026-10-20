# Calculate Actuarial Present Value (APV) of a Loan

Calculates the APV of a loan under competing risks (default vs.
prepayment) and left-truncated survival data.

## Usage

``` r
calculate_apv(
  x,
  cur_age,
  orig_term,
  orig_loan_amt,
  mon_pmt,
  ref_rate,
  recov_curve,
  orig_apy = 0.15,
  type = "constant"
)
```

## Arguments

- x:

  An object of class `alife_multi`. Typically the output of the
  `estimate_hazard` function with two event types.

- cur_age:

  Current age of the loan in months.

- orig_term:

  Original loan term in months.

- orig_loan_amt:

  Original loan amount.

- mon_pmt:

  Monthly payment amount.

- ref_rate:

  Annualized reference discount rate.

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

An object of class `apv`: a one-row `data.frame` with the APV (`APV`),
and its standard deviation (`SD`).
[`rbind()`](https://rdrr.io/r/base/cbind.html) stacks several `apv`
objects into a `data.frame`; named arguments become row names.

## Details

`x` must contain exactly two event types, and the default event must be
labeled either `"Default"` or `"1"`; the other event type is treated as
prepayment. If the observed lifetime support does not reach `orig_term`,
the hazards are extrapolated up to `orig_term` with
[`extend_hazard()`](http://lcgodoy.me/abslife/reference/extend_hazard.md)
(with a warning). After that, hazard estimates must be available for
every lifetime from `cur_age + 1` to `orig_term`; otherwise, an error is
thrown.
