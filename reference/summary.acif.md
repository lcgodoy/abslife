# Summary Method for an 'acif' Object

Summary Method for an 'acif' Object

## Usage

``` r
# S3 method for class 'acif'
summary(object, by = 5, ...)
```

## Arguments

- object:

  An object of class `acif`. Typically the output of the `calc_cif`
  method.

- by:

  an `integer` defining the periodicity of the summary.

- ...:

  Not used.

## Value

A `data.frame` with the cause-specific CIFs and the all-cause survival
at every `by` time points.

## See also

[`calc_cif()`](http://lcgodoy.me/abslife/reference/calc_cif.md)
