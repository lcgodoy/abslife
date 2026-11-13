# Calculate Cumulative Incidence Functions (CIF) from estimated hazard rates

Computes the all-cause survival function and the cause-specific
cumulative incidence functions from cause-specific hazard estimates. An
object of class `acdf_multi` is returned. Using `calc_cif()` with
`alife` object will generate an error. Instead, use
[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md) when no
competing risks are present.

## Usage

``` r
calc_cif(x, ...)

# S3 method for class 'alife_multi'
calc_cif(x, ...)
```

## Arguments

- x:

  An object of class `alife_multi`. Typically the output of the
  `estimate_hazard` function with more than one event type.

- ...:

  Not used.

## Value

An object of class `acif`, a `data.frame` with columns `event_type`,
`lifetime`, `all_surv` (all-cause survival up to the previous time
point), `pr_zx` (probability of the event of a given type at
`lifetime`), `cif`, and `risk_set`.

## See also

[`calc_cdf()`](http://lcgodoy.me/abslife/reference/calc_cdf.md)
