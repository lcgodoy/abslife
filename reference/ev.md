# Calculates expected value from an estimated lifetime distribution

For a [`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md)
estimated density function (an `acdf` object), this function computes
the expected value of a lifetime. For a
[`calc_cif()`](http://lcgodoy.me/abslife/reference/calc_cif.md)
estimated cause-specific density function (an `acdf_multi`) object, this
function computes the cause-specific and all-cause expected lifetimes.
Alternatively, a user may directly input an a `alife` object from
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md).

## Usage

``` r
ev_life(x, digits = 2, ...)

# S3 method for class 'acdf'
ev_life(x, digits = 2, ...)

# S3 method for class 'acdf_multi'
ev_life(x, digits = 2, ...)

# S3 method for class 'acif'
ev_life(x, digits = 2, ...)

# S3 method for class 'alife'
ev_life(x, digits = 2, ...)
```

## Arguments

- x:

  An object of class `acdf`, `acdf_multi`, or `alife`.

- digits:

  number of digits for the printed output.

- ...:

  Not used.

## Value

The expected value associated with the object `x` (printed and returned
invisibly). For `acdf_multi` objects, a named vector with the all-cause
and the cause-specific expected lifetimes.
