# Summary Method for an 'alife_multi' Object

Summary Method for an 'alife_multi' Object

## Usage

``` r
# S3 method for class 'acdf_multi'
summary(object, by = 5, ...)
```

## Arguments

- object:

  An object of class `alife`. Typically the output of the
  `estimate_hazard` function.

- by:

  an `integer` defining the periodicity of the summary.

- ...:

  Additional arguments passed to the base `print` function (e.g.,
  `digits`).

## Value

A summary of the hazard rate.

## See also

[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
