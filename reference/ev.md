# Calculate expected value for the time to event of interest

Uses the estimated density from the 'acdf' output.

## Usage

``` r
ev_life(x, digits = 2, ...)

# S3 method for class 'acdf'
ev_life(x, ...)

# S3 method for class 'alife'
ev_life(x, ...)
```

## Arguments

- x:

  An object of class `acdf`.

- digits:

  number of digits for the output.

- ...:

  extra arguments to be passed to the `round` function.

## Value

The expected value associated with the object `x`.
