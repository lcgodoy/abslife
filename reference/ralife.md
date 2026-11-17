# Generates a random sample from an estimated lifetime distribution

For an estimated lifetime distribution (i.e., an `acdf`, `acdf_multi`,
or `acif` object) generated from
[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md) or
[`calc_cif()`](http://lcgodoy.me/abslife/reference/calc_cif.md), this
function will generate `n` random samples.

## Usage

``` r
ralife_cdf(n, x)

ralife_cif(n, x)
```

## Arguments

- n:

  number of observations.

- x:

  the output of a
  [`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md) call
  (an `acdf` or `acdf_multi` object) or a
  [`calc_cif()`](http://lcgodoy.me/abslife/reference/calc_cif.md) cal
  (an `acif` object).

## Value

a vector of "time to event" samples or, if `x` is an `acdf_multi` or
`acif` object, a `data.frame` with the sampled `event_type` and
`lifetime`.
