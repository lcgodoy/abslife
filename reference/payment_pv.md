# Internal helper for payment present value

Internal helper for payment present value

## Usage

``` r
payment_pv(
  payment,
  lifetime,
  delay,
  elapsed_time,
  payment_cap,
  discount_factors
)
```

## Arguments

- payment:

  Monthly payment.

- lifetime:

  Time of termination.

- delay:

  Delay in payment.

- elapsed_time:

  Current age.

- payment_cap:

  Cap on payments.

- discount_factors:

  Vector of discount factors.
