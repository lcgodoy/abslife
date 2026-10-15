# Calculate expected value for the time to event of interest

Uses the estimated density from the 'acdf' output.

## Usage

``` r
ev_life(x, digits = 2, ...)

# S3 method for class 'acdf'
ev_life(x, digits = 2, ...)

# S3 method for class 'acdf_multi'
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
