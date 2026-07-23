# Consumer automobile loans recovery curves

A dataset containing the recovery curves associated with the automobile
loans analyzed in Lautier et al. 2024.

## Usage

``` r
aloans_recov
```

## Format

### `aloans_recov`

A data frame with 365 rows and 3 columns:

- risk.cat:

  Credit risk band based on the loan's Annual Percentage Rate (APR).
  Categories include "super_prime" (0-5%), "prime" (5-10%), "near_prime"
  (10-15%), "subprime" (15-20%), and "deep_subprime" (20%+).

- month:

  The month for which the recovery is estimated.

- recovery:

  The estimated recovery rate for a given month and credit risk band.

## References

Lautier, J. P., Pozdnyakov, V., & Yan, J. (2024). On the convergence of
credit risk in current consumer automobile loans. *Journal of the Royal
Statistical Society Series A: Statistics in Society*, qnae137.
[doi:10.1093/jrsssa/qnae137](https://doi.org/10.1093/jrsssa/qnae137) .
