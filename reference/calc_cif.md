# Calculate Cumulative Incidence Functions (CIF) from Hazard Estimates

Computes the all-cause survival function and the cause-specific
cumulative incidence functions from cause-specific hazard estimates.

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

[`plot.acif()`](http://lcgodoy.me/abslife/reference/plot.acif.md),
[`summary.acif()`](http://lcgodoy.me/abslife/reference/summary.acif.md)
