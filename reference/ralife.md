# Generates a random sample from an estimated lifetime distribution

Draws `n` random lifetimes from the distribution estimated by
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md),
[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md), or
[`calc_cif()`](http://lcgodoy.me/abslife/reference/calc_cif.md). For
competing risks, each draw also has an event type.

## Usage

``` r
ralife(n, x, ...)

# S3 method for class 'acdf'
ralife(n, x, ...)

# S3 method for class 'acdf_multi'
ralife(n, x, ...)

# S3 method for class 'acif'
ralife(n, x, ...)

# S3 method for class 'alife'
ralife(n, x, ...)

# S3 method for class 'alife_multi'
ralife(n, x, ...)

# Default S3 method
ralife(n, x, ...)
```

## Arguments

- n:

  number of samples.

- x:

  an object of class `alife`, `alife_multi`, `acdf`, `acdf_multi`, or
  `acif`. Typically the output of
  [`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md),
  [`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md), or
  [`calc_cif()`](http://lcgodoy.me/abslife/reference/calc_cif.md).

- ...:

  not used.

## Value

For `alife` and `acdf` objects, a vector of `n` sampled "times to
event". For `alife_multi`, `acdf_multi`, and `acif` objects, a
`data.frame` with `n` rows and the columns `event_type` and `lifetime`
("time to event").

## See also

[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md),
[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md),
[`calc_cif()`](http://lcgodoy.me/abslife/reference/calc_cif.md)

## Author

lcgodoy

## Examples

``` r
aart_hazard <- estimate_hazard(lifetime = aart$Xi,
                               trunc_time = aart$Yi,
                               carry_hazard = TRUE)
set.seed(1)
ralife(n = 10, x = aart_hazard)
#>  [1] 19 25 36 40 15 40 40 39 38  8
ralife(n = 10, x = calc_cdf(aart_hazard))
#>  [1] 16 14 39 26 39 33 39 40 26 39
```
