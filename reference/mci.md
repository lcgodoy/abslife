# Computing multiple CIs at once.

Computing multiple CIs at once.

## Usage

``` r
multiple_cis(x, ci_level = 0.95)

# S3 method for class 'alife'
multiple_cis(x, ci_level = 0.95)

# S3 method for class 'alife_multi'
multiple_cis(x, ci_level = 0.95)
```

## Arguments

- x:

  An object of class `alife`. Typically the output of the
  `estimate_hazard` function.

- ci_level:

  A numeric vector of confidence ci_level to plot (e.g.,
  `c(0.5, 0.95)`). Defaults to `0.95`.

## Value

a `list`

## Author

lcgodoy
