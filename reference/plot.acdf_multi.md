# Plot Method for an 'acdf_multi' Object

Creates a faceted plot with one row per event type. Each row contains
two panels: one for the cause-specific Cumulative Incidence Function
(CIF) and one for the Probability Mass Function (Density).

## Usage

``` r
# S3 method for class 'acdf_multi'
plot(x, ci_level = 0.95, color = 2, col_line = 1, ...)
```

## Arguments

- x:

  An object of class `acdf_multi`.

- ci_level:

  Currently ignored: standard errors are not yet available for competing
  risks, so no confidence intervals are drawn.

- color:

  Currently ignored (see `ci_level`).

- col_line:

  The color for the main estimate line/points.

- ...:

  Additional arguments passed to the base `plot` function (e.g., `xlab`,
  `ylab` override).

## Value

A faceted plot of CIFs and Densities.

## See also

[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md)
