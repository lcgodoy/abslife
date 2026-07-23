# Solve for Risk-Adjusted Internal Rate of Return (IRR)

Solve for Risk-Adjusted Internal Rate of Return (IRR)

## Usage

``` r
solve_irr(
  abs0_bal,
  cur_age,
  orig_term,
  orig_loan_amt,
  mon_pmt,
  recov_curve,
  prbs,
  orig_apy = 0.15
)
```

## Arguments

- abs0_bal:

  Current loan balance (to match EPV against).

- cur_age:

  Current age of the loan in months.

- orig_term:

  Original loan term in months.

- orig_loan_amt:

  Original loan amount.

- mon_pmt:

  Monthly payment amount.

- recov_curve:

  Recovery curve data frame.

- prbs:

  CDF and density probabilities from abslife.

- orig_apy:

  Internal loan APY for amortization. Defaults to 0.15.

## Value

List with risk adjusted monthly rate, annualized rate, and objective
value.
