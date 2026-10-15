# Validate an acif Object

Checks if an object is a valid `acif` object by ensuring it is a data
frame and contains all required columns.

## Usage

``` r
validate_acif(x)
```

## Arguments

- x:

  An object to validate.

## Value

`NULL`, invisibly, if validation is successful. Throws an error on
failure.
