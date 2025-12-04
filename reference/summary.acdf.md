# Summary Method for an 'acdf' Object

Summary Method for an 'acdf' Object

## Usage

``` r
# S3 method for class 'acdf'
summary(object, by = 5, ...)
```

## Arguments

- object:

  An object of class `acdf`. Typically the output of the
  `estimate_hazard` function.

- by:

  an `integer` defining the periodicity of the summary.

- ...:

  Additional arguments passed to the base `print` function (e.g.,
  `digits`).

## Value

A summary of the CDF function.

## See also

[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md)
