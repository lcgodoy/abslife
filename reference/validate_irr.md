# Validate an irr Object

Checks if an object is a valid `irr` object by checking if it is a data
frame and contains all required columns.

## Usage

``` r
validate_irr(x)
```

## Arguments

- x:

  An object to validate.

## Value

`NULL`, invisibly, if validation is successful. Throws an error on
failure.
