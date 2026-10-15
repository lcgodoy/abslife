# Summary Method for an 'acdf_multi' Object

Summary Method for an 'acdf_multi' Object

## Usage

``` r
# S3 method for class 'acdf_multi'
summary(object, by = 5, ...)
```

## Arguments

- object:

  An object of class `acdf_multi`. Typically the output of the
  `calc_cdf` method.

- by:

  an `integer` defining the periodicity of the summary.

- ...:

  Not used.

## Value

A `data.frame` with the all-cause survival, cause-specific cumulative
incidence, and density at every `by` time points.

## See also

[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md)
