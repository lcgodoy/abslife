# Prepare alife object for ggplot

Helper (used by
[`ggauto()`](http://lcgodoy.me/abslife/reference/ggauto.md)) to create a
long-form data frame with multiple confidence intervals for plotting. As
in
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md),
the intervals are computed at the logit scale.

## Usage

``` r
prep_ggplot(x, ci_level = 0.95)
```

## Arguments

- x:

  An object of class `alife` or `alife_multi`.

- ci_level:

  A numeric vector of confidence levels (e.g., `c(0.80, 0.95)`).

## Value

A long-form `data.frame` with CI columns.
