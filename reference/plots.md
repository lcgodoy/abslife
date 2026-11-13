# Plot method for an `alife` or `alife_multi` object

This function allows for an `alife` object to be fed into
[`plot()`](https://rdrr.io/r/graphics/plot.default.html). For
`alife_multi` objects, it creates a faceted plot, with one panel per
event type.

## Usage

``` r
# S3 method for class 'alife'
plot(x, ci_level = 0.95, color = 2, col_line = 1, ...)

# S3 method for class 'alife_multi'
plot(x, ci_level = 0.95, color = 2, col_line = 1, ...)
```

## Arguments

- x:

  An object of class `alife` or `alife_multi`. Typically the output of
  the `estimate_hazard` function.

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

A plot of the hazard rate (`alife`) or a faceted plot of the hazard
rates (`alife_multi`).

## See also

[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
