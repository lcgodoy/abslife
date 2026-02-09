# Prepare alife object for ggplot

Internal helper to create a long-form data frame with multiple
confidence intervals for plotting.

## Usage

``` r
prep_ggplot(x, ci_level = 0.95)
```

## Arguments

- x:

  An object of class 'alife' or 'alife-multi'

- ci_level:

  A numeric vector of confidence levels (e.g., c(0.80, 0.95))

## Value

A long-form data.frame with CI columns.
