# Calculates a full loan amortization schedule

This function produces a complete amortization schedule from a loan
contract's terms. It is used within
[`calculate_apv()`](http://lcgodoy.me/abslife/reference/calculate_apv.md)
to determine prepayment amounts, but it may be used as a standalone
function.

## Usage

``` r
amort_bal(time, orig_bal, int_rate, payment)
```

## Arguments

- time:

  A complete vector of payment periods (e.g., for a 60-month loan, input
  `c(1:60)`).

- orig_bal:

  The original loan balance.

- int_rate:

  The effective interest rate that compounds at the same frequency of
  the payment periods.

- payment:

  The payment due at each payment period (e.g., the monthly payment for
  a 60-month auto loan).

## Value

The outstanding balance at each month over the life of the loan assuming
no payment uncertainty (i.e., the full amortization schedule).

## Details

For a 12% annual percentage rate (APR) on a loan with monthly payments,
`int_rate` should be `(1.12)^(1/12) - 1`.
