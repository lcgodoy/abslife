# Plots an `alife` (or `alife_multi`) object with `ggplot2`

Reproduces plotting functionality from
[`plots()`](http://lcgodoy.me/abslife/reference/plots.md) but with the
`ggplot2` package for an alternative plotting device. The output is a
plot of the hazard rate (if an `alife` object) or a faceted plot of the
hazard rates (if an `alife_multi` object).

## Usage

``` r
ggauto(object, ...)

# S3 method for class 'alife'
ggauto(object, ci_level = 0.95, ...)

# S3 method for class 'alife_multi'
ggauto(object, ci_level = 0.95, ...)
```

## Arguments

- object:

  An object of class `alife` or `alife_multi` produced by
  [`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md).

- ...:

  Additional arguments (not used).

- ci_level:

  A single number or numeric vector of confidence levels to display
  (e.g., `0.95` or `c(0.80, 0.95)`).

## Value

A `ggplot` object that may be customized accordingly (see
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)).
