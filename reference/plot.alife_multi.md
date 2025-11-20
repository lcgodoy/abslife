# Plot Method for an 'alife_multi' Object

Creates a faceted plot, with one panel per event type.

## Usage

``` r
# S3 method for class 'alife_multi'
plot(x, col_ci = "skyblue", col_line = "navy", ...)
```

## Arguments

- x:

  An object of class `alife-multi`.

- col_ci:

  The color for the confidence interval polygon.

- col_line:

  The color for the hazard rate line.

- ...:

  Additional arguments passed to the base `plot` function (e.g., `xlab`,
  `ylab`, `ylim`).

## Value

A faceted plot of the hazard rates.

## See also

[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
