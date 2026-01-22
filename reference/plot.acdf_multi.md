# Plot Method for an 'acdf_multi' Object

Creates a faceted plot, with one panel per event type.

Creates a faceted plot with one row per event type. Each row contains
two panels: one for the Cumulative Distribution Function (CDF) and one
for the Probability Mass Function (Density).

## Usage

``` r
# S3 method for class 'acdf_multi'
plot(x, ci_level = 0.95, col_ci = "grey80", col_line = 1, ...)

# S3 method for class 'acdf_multi'
plot(x, ci_level = 0.95, col_ci = "grey80", col_line = 1, ...)
```

## Arguments

- x:

  An object of class `acdf_multi`.

- ci_level:

  A numeric vector of confidence levels to plot (e.g., `c(0.5, 0.95)`).
  Defaults to `0.95`.

- col_ci:

  The color for the confidence interval polygon/bars.

- col_line:

  The color for the main estimate line/points.

- ...:

  Additional arguments passed to the base `plot` function (e.g., `xlab`,
  `ylab` override).

## Value

A faceted plot of the CDFs.

A faceted plot of CDFs and Densities.

## See also

[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md)

[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md)
