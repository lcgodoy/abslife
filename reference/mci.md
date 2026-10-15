# Computing multiple CIs at once

Computing multiple CIs at once

## Usage

``` r
multiple_cis(x, ci_level = 0.95)

# S3 method for class 'alife'
multiple_cis(x, ci_level = 0.95)

# S3 method for class 'alife_multi'
multiple_cis(x, ci_level = 0.95)

# S3 method for class 'acdf'
multiple_cis(x, ci_level = 0.95)

# S3 method for class 'acdf_multi'
multiple_cis(x, ci_level = 0.95)
```

## Arguments

- x:

  An object of class `alife`, `alife_multi`, `acdf`, or `acdf_multi`.

- ci_level:

  A numeric vector of confidence levels (e.g., `c(0.5, 0.95)`). Defaults
  to `0.95`.

## Value

a `list` with one `data.frame` of confidence limits per element of
`ci_level`.

## Author

lcgodoy
