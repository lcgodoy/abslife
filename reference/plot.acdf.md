# Plot Method for an 'acdf' Object

Plot Method for an 'acdf' Object

## Usage

``` r
# S3 method for class 'acdf'
plot(x, ci_level = 0.95, col_ci = "grey80", col_line = 1, ...)
```

## Arguments

- x:

  An object of class `acdf`. Typically the output of the
  `estimate_hazard` function.

- ci_level:

  A numeric vector of confidence ci_level to plot (e.g.,
  `c(0.5, 0.95)`). Defaults to `0.95`.

- col_ci:

  The color for the confidence interval polygon.

- col_line:

  The color for the hazard rate line.

- ...:

  Additional arguments passed to the base `plot` function (e.g., `main`,
  `xlab`, `ylab`, `ylim`).

## Value

A plot of the CDF.

## See also

[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md)
