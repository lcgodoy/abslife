# Calculate Actuarial Present Value (APV) of a Loan

Calculates the APV of a loan under competing risks (Default vs.
Pre-payment) and left-truncated survival data.

## Usage

``` r
calculate_apv(
  cur_age,
  orig_term,
  orig_loan_amt,
  mon_pmt,
  ref_rate,
  recov_curve,
  prbs,
  orig_apy = 0.15
)
```

## Arguments

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

  Recovery curve data frame (columns: Month, Recovery).

- prbs:

  CDF and density probabilities from abslife (competing risks).

- orig_apy:

  Internal loan APY for amortization. Defaults to 0.15.

## Value

A list containing APV, second moment (APV2), standard deviation (SD),
survival probability to current age (S_cur_age), and conditional
probability of survival to maturity (cond_p_mat).
