# Plot Method for an 'acif' Object

Creates plots for cause-specific cumulative incidence functions (CIF),
all-cause survival function, or both.

## Usage

``` r
# S3 method for class 'acif'
plot(x, which = c("cif", "all_surv", "both"), col_line = 1, lwd = 2, ...)
```

## Arguments

- x:

  An object of class `acif`. Typically the output of the `calc_cif`
  function.

- which:

  A character string specifying what to plot: `"cif"` for cause-specific
  cumulative incidence functions (one panel per event type),
  `"all_surv"` for the all-cause survival function, or `"both"` for
  all-cause survival and cause-specific CIFs. Defaults to `"cif"`.

- col_line:

  Color for the lines. Defaults to `1` (black).

- lwd:

  Line width. Defaults to `2`.

- ...:

  Additional arguments passed to the base `plot` function (e.g., `xlab`,
  `ylab`, `ylim`, `xlim`).

## Value

Invisibly returns the input object `x`.

## See also

[`calc_cif()`](http://lcgodoy.me/abslife/reference/calc_cif.md)
