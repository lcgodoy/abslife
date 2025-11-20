# Calculate CDF from Hazard Estimates

Adds a 'cdf' column to an 'alife' or 'alife-multi' object based on the
hazard estimates.

## Usage

``` r
calc_cdf(x, ...)

# S3 method for class 'alife'
calc_cdf(x, ...)

# S3 method for class 'alife_multi'
calc_cdf(x, ...)
```

## Arguments

- x:

  An object of class `alife` or `alife-multi`.

- ...:

  Not used.

## Value

The original object `x` with a new `cdf` column.
