# Calculate CDF from Hazard Estimates

Returns an object of class `acdf`, corresponding to a `data.frame` with
tailored plot and summary methods. For competing risks (`alife_multi`
objects), the cause-specific cumulative incidence functions are computed
instead and an object of class `acdf_multi` is returned (see also
[`calc_cif()`](http://lcgodoy.me/abslife/reference/calc_cif.md)).

## Usage

``` r
calc_cdf(x, ...)

# S3 method for class 'alife'
calc_cdf(x, ...)

# S3 method for class 'alife_multi'
calc_cdf(x, ...)
```

## Arguments

- x:

  An object of class `alife` or `alife_multi`.

- ...:

  Not used.

## Value

An object of class `acdf` (columns `lifetime`, `cdf`, `se_cdf`,
`density`, `se_dens`, and `risk_set`) or, if `x` is an `alife_multi`
object, of class `acdf_multi` (columns `event_type`, `lifetime`,
`all_surv`, `cumulative`, `density`, and `risk_set`).
