# Plot Method for an 'acdf' Object

Creates plots for cause-specific cumulative incidence functions (CIF),
all-cause survival function, or both.

Creates a faceted plot with one row per event type. Each row contains
two panels: one for the cause-specific Cumulative Incidence Function
(CIF) and one for the Probability Mass Function (Density).

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

- which:

  A character string specifying what to plot: `"cif"` for cause-specific
  cumulative incidence functions (one panel per event type),
  `"all_surv"` for the all-cause survival function, or `"both"` for
  all-cause survival and cause-specific CIFs. Defaults to `"cif"`.

- lwd:

  Line width. Defaults to `2`.

## Value

A plot of the CDF.

Invisibly returns the input object `x`.

A faceted plot of CIFs and Densities.

## See also

[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md)

[`calc_cif()`](http://lcgodoy.me/abslife/reference/calc_cif.md)
