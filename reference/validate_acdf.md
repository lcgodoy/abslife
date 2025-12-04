# Validate an acdf Object

Checks if an object is a valid `acdf` object by ensuring it is a data
frame and contains all required columns.

## Usage

``` r
validate_acdf(x)
```

## Arguments

- x:

  An object to validate.

## Value

The input `x`, invisibly, if validation is successful. Throws an error
on failure.
