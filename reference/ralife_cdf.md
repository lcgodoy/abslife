# Samples from an `acdf` (or `acdf_multi`) object

Samples from an `acdf` (or `acdf_multi`) object

## Usage

``` r
ralife_cdf(n, x)
```

## Arguments

- n:

  number of samples

- x:

  the output of a
  [`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md) call
  (an `acdf` or `acdf_multi` object).

## Value

a vector of "time to event" samples or, if `x` is an `acdf_multi`
object, a `data.frame` with the sampled `event_type` and `lifetime`.

## Author

lcgodoy
