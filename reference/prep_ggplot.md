# Prepares an `alife` or `alife_multi` object for `ggauto`

This is a helper function that is used by
[`ggauto()`](http://lcgodoy.me/abslife/reference/ggauto.md)) to create a
long-form data frame with multiple confidence intervals suitable for
plotting. To be consistent with
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md),
the confidence intervals are computed on the logit scale.

## Usage

``` r
prep_ggplot(x, ci_level = 0.95)
```

## Arguments

- x:

  An object of class `alife` or `alife_multi` produced by
  [`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md).

- ci_level:

  A numeric vector of confidence levels (e.g., `c(0.80, 0.95)`).

## Value

A long-form `data.frame` with confidence interval columns.
