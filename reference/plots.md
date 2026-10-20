# Plot Method for an 'alife' Object

Creates a faceted plot, with one panel per event type.

## Usage

``` r
# S3 method for class 'alife'
plot(x, ci_level = 0.95, color = 2, col_line = 1, ...)

# S3 method for class 'alife_multi'
plot(x, ci_level = 0.95, color = 2, col_line = 1, ...)
```

## Arguments

- x:

  An object of class `alife_multi`.

- ci_level:

  A numeric vector of confidence levels to plot (e.g., `c(0.5, 0.95)`).
  Defaults to `0.95`.

- color:

  The color for the confidence interval polygon.

- col_line:

  The color for the hazard rate line.

- ...:

  Additional arguments passed to the base `plot` function (e.g., `xlab`,
  `ylab`, `ylim`).

## Value

A plot of the hazard rate.

A faceted plot of the hazard rates.

## See also

[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
