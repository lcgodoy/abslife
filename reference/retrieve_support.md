# Computes the observable support of a lifetime variable

This helper function generates the observable supports of the lifetime
and left-truncation random variables (Lautier et al. 2025,
[doi:10.1214/25-AOAS2103](https://doi.org/10.1214/25-AOAS2103) ).

## Usage

``` r
retrieve_support(Delta, m, omega, epsilon = NULL)
```

## Arguments

- Delta:

  a nonnegative integer denoting the period of time (in months) during
  which the ABS is marketed to prospective investors. It is after the
  trust closes to new loan originations but before its first reporting
  month.

- m:

  is a positive integer representing the total number of months over
  which the underlying consumer auto loans are originated. This defines
  the length of the loan origination window before the trust closes.

- omega:

  is a positive integer denoting the known, finite upper bound of a
  loan's lifetime (in months), which is nonrandom and observable from
  its amortization schedule at contract signing (for example, for a
  72-month loan, we likely have \\\omega = 72\\).

- epsilon:

  is a positive integer (default is `NULL`) denoting the present time
  (the current calendar month age since the first loan was originated)
  of the data generation process for an active ABS pool. If
  \\\varepsilon \< \omega + m\\, then right-censoring is present because
  it indicates some active loans are still making ongoing payments and
  have not yet terminated.

## Value

A list with two integer vectors: `X`, the observable support of the
lifetime variable (from \\\Delta + 1\\ to \\\xi\\), and `Y`, the support
of the left-truncation variable (from \\\Delta + 1\\ to \\\Delta + m\\).

## Note

The observed support may not extend all the way to \\\omega\\ (`omega`).
In that scenario, we set \\\omega\\ to the age of the oldest active loan
in the ABS bond (corresponding to \\\xi \equiv \min(\omega,
\varepsilon - 1)\\ in the notation used in Lautier et al. 2025), with
\\\varepsilon\\ denoting the present time.
