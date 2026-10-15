# Dimensions of Observable Support

This helper function returns the observable support of the lifetime and
left-truncation random variables (e.g., Lautier et al., 2026).

## Usage

``` r
retrieve_support(Delta, m, omega, epsilon = NULL)
```

## Arguments

- Delta:

  A nonnegative integer denoting the period of time (in months) during
  which the ABS is marketed to prospective investors. It is after the
  trust closes to new loan originations but before its first reporting
  month.

- m:

  A positive integer representing the total number of months over which
  the underlying consumer auto loans are originated. This defines the
  length of the loan origination window before the trust closes.

- omega:

  A positive integer denoting the known, finite upper bound of a loan's
  lifetime (in months), which is nonrandom and observable from its
  amortization schedule at contract signing (for example, for a 72-month
  loan, we likely have \\\omega = 72\\).

- epsilon:

  A positive integer (default is `NULL`) denoting the present time (the
  current calendar month age since the first loan was originated) of the
  data generation process for an active ABS pool. If \\\varepsilon \<
  \omega + m\\, then right-censoring is present because it indicates
  some active loans are still making ongoing payments and have not yet
  terminated (Lautier et al., 2023)

## Value

A list with two integer vectors: `X`, the observable support of the
lifetime variable (from \\\Delta + 1\\ to \\\xi\\), and `Y`, the support
of the left-truncation variable (from \\\Delta + 1\\ to \\\Delta + m\\).

## Note

The observed support may not extend all the way to \\\omega\\ (`omega`).
In that scenario, we set \\\omega\\ to the age of the oldest active loan
in the ABS bond (corresponding to \\\xi \equiv \min(\omega,
\varepsilon - 1)\\ in the notation used in Lautier et al., 2023), with
\\\varepsilon\\ denoting the present time.

## References

Lautier, J. P., Pozdnyakov, V., Yan, J. (2023). Pricing time-to-event
contingent cash-flows: A discrete-time survival analysis approach.
*Insurance: Mathematics and Economics*, 110, 53-71.
<https://doi.org/10.1016/j.insmatheco.2023.02.003>

Lautier, J. P., Pozdnyakov, V., Yan, J. (2026). Estimating a discrete
distribution subject to random left-truncation with an application to
structured finance. *Econometrics and Statistics*, 37, 174-198.
<https://doi.org/10.1016/j.ecosta.2023.05.005>
