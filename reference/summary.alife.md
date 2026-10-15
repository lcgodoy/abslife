# Summary Method for an 'alife' Object

Summary Method for an 'alife' Object

## Usage

``` r
# S3 method for class 'alife'
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

Prints the hazard estimates at every `by` time points and invisibly
returns them as a `data.frame`.

## See also

[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
