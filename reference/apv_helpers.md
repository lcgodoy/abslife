# Set up and evaluate the APV of a loan (internal use)

`.apv_setup()` checks the inputs of
[`calculate_apv()`](http://lcgodoy.me/abslife/reference/calculate_apv.md)
and [`solve_irr()`](http://lcgodoy.me/abslife/reference/solve_irr.md)
and computes everything that does not depend on the discount rate (event
probabilities and cash flows). `.apv_moments()` computes the APV, its
second moment, and its standard deviation for a given monthly discount
rate.

## Usage

``` r
.apv_setup(
  x,
  cur_age,
  orig_term,
  orig_loan_amt,
  mon_pmt,
  recov_curve,
  orig_apy,
  type
)

.apv_moments(setup, rate)
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

- setup:

  The output of `.apv_setup()`.

- rate:

  A monthly discount rate.

## Value

`.apv_setup()` returns a list with the number of remaining months, the
default and prepayment probabilities, and the cash flows for each
remaining month. `.apv_moments()` returns a list with the APV (`APV`)
and its standard deviation (`SD`).
