# Plot method for an `acdf`, `acdf_multi`, or `acif` object

This function allows for an `acdf` object to be fed into
[`plot()`](https://rdrr.io/r/graphics/plot.default.html). The default
display is the estimated cumulative density function (CDF) and
probability mass function (PMF) with associated confidence bands. For
`acdf_multi` objects, it creates a faceted plot, with one panel for
cause-specific cumulative incidence functions (CIF) and estimated
density functions for each event type. For `acif` objects, it returns a
faceted plot with the CIF function for each event type.

## Usage

``` r
# S3 method for class 'acdf'
plot(x, ci_level = 0.95, color = 2, col_line = 1, ...)

# S3 method for class 'acif'
plot(x, which = c("cif", "all_surv", "both"), col_line = 1, lwd = 2, ...)

# S3 method for class 'acdf_multi'
plot(x, ci_level = 0.95, color = 2, col_line = 1, ...)
```

## Arguments

- x:

  An object of class `acdf`, `acdf_multi`, or `acif`. Typically the
  output of the `calc_cdf` or `calc_cif` functions.

- ci_level:

  A numeric vector of confidence levels to plot (e.g., `c(0.5, 0.95)`).
  Defaults to `0.95`. For `acdf_mult` or `acif` objects, confidence
  intervals are not returned.

- color:

  The color for the confidence interval polygon for the cumulative
  density function and error bars for the density function. For
  `acdf_mult` or `acif` objects, confidence intervals are not returned.

- col_line:

  The color for the CDF (CIF) line(s) and density points. Defaults to
  `1` (black).

- ...:

  Additional arguments passed to the base `plot` function (e.g., `main`,
  `xlab`, `ylab`, `ylim`).

- which:

  A character string specifying what to plot: `"cif"` for cause-specific
  cumulative incidence functions (one panel per event type),
  `"all_surv"` for the all-cause survival function, or `"both"` for
  all-cause survival and cause-specific CIFs. Defaults to `"cif"`.

- lwd:

  Line width. Defaults to `2`.

## Value

For `acdf` objects, returns the CDF and PMF with associated confidence
bands. For `acdf_multi` objects, returns the CIF and associated density
for each event type. For `acif` objects, returns the CIF for each event
type. Also invisibly returns the input object `x`.

## See also

[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md)

[`calc_cif()`](http://lcgodoy.me/abslife/reference/calc_cif.md)
