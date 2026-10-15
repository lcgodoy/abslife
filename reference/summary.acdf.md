# Summary Method for an 'acdf' Object

Summary Method for an 'acdf' Object

## Usage

``` r
# S3 method for class 'acdf'
summary(object, by = 5, ...)
```

## Arguments

- object:

  An object of class `acdf`. Typically the output of the `calc_cdf`
  method.

- by:

  an `integer` defining the periodicity of the summary.

- ...:

  Not used.

## Value

A `data.frame` with the CDF and density at every `by` time points.

## See also

[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md)
