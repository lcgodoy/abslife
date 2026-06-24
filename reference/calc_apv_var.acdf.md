# Calculate Variance of Actuarial Present Value

Computes the variance of the Actuarial Present Value (APV) of future
cash flows.

## Usage

``` r
# S3 method for class 'acdf'
calc_apv_var(
  x,
  payment,
  residual_value,
  elapsed_time,
  max_lifetime,
  max_delay,
  payment_cap,
  discount_factors,
  delay_prob_matrix,
  depreciation_matrix,
  ...
)
```

## Arguments

- x:

  An object of class `acdf` or `alife`.

- payment:

  Monthly payment amount.

- residual_value:

  Contractual residual value.

- elapsed_time:

  Current age (time elapsed) of the lease.

- max_lifetime:

  Maximum time to consider (time horizon).

- max_delay:

  Maximum possible payment delay.

- payment_cap:

  Cap on the number of payments to be made.

- discount_factors:

  Numeric vector of monthly discount factors.

- delay_prob_matrix:

  Matrix where rows are termination times and columns are delay lengths.

- depreciation_matrix:

  Matrix with rows as termination times, first column is expectation and
  second column is variance of depreciation.

- ...:

  Additional arguments.
