# Validate an apv Object

Checks if an object is a valid `apv` object by checking if it is a data
frame and contains all required columns.

## Usage

``` r
validate_apv(x)
```

## Arguments

- x:

  An object to validate.

## Value

`NULL`, invisibly, if validation is successful. Throws an error on
failure.
