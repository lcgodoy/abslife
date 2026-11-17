# Calculate discount factors for an effective interest rate

This function produces a vector of discount factors from a timeline and
effective interest rate. It is used within
[`calculate_apv()`](http://lcgodoy.me/abslife/reference/calculate_apv.md)
to discount future payments, but it may be used as a standalone
function.

## Usage

``` r
disc_fac(time, int_rate)
```

## Arguments

- time:

  A complete vector of payment periods (e.g., for a 60-month loan, input
  `c(1:60)`).

- int_rate:

  The effective interest rate that compounds at the same frequency of
  the payment periods.

## Value

A vector of discount factors corresponding to `time`.

## Details

For a 12% annual percentage rate (APR) on a loan with monthly payments,
`int_rate` should be `(1.12)^(1/12) - 1`.
