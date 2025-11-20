# Plot an 'alife-multi' Object using `ggplot2`

Creates a faceted plot, with one panel per event type.

## Usage

``` r
# S3 method for class 'alife_multi'
ggauto(object, ci_level = 0.95, ...)
```

## Arguments

- object:

  An object of class `alife-multi`.

- ci_level:

  A single number or numeric vector of confidence levels to display
  (e.g., `0.95` or `c(0.80, 0.95)`).

- ...:

  Additional arguments (not used).

## Value

A `ggplot` object.
