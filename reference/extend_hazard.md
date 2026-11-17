# Extrapolate hazard rates beyond observable region

This function will extrapolate an estimated hazard rate object from
[`estimate_hazard()`](http://lcgodoy.me/abslife/reference/estimate_hazard.md)
(i.e., an `alife` or `alife_multi` object) up to a specified `end` time
using a constant, geometric, or linear extrapolation. Also sets the
(all-cause) hazard at the specified `end` time to 1. This function is
used by
[`calculate_apv()`](http://lcgodoy.me/abslife/reference/calculate_apv.md)
and [`solve_irr()`](http://lcgodoy.me/abslife/reference/solve_irr.md)
when the observed lifetime support does not reach the original loan term
and an assumption is needed.

## Usage

``` r
extend_hazard(
  x,
  end = 72,
  type = c("constant", "geometric", "linear"),
  end_event = "Prepayment"
)

# S3 method for class 'alife'
extend_hazard(
  x,
  end = 72,
  type = c("constant", "geometric", "linear"),
  end_event = "Prepayment"
)

# S3 method for class 'alife_multi'
extend_hazard(
  x,
  end = 72,
  type = c("constant", "geometric", "linear"),
  end_event = "Prepayment"
)
```

## Arguments

- x:

  An object of class `alife` or `alife_multi`.

- end:

  A numeric scalar indicating the target end time for the support.
  Typically, the original loan term. Must be greater than the largest
  observed lifetime.

- type:

  A character string specifying the extrapolation type: either
  `"constant"` (default), `"geometric"` or `"linear"`.

- end_event:

  A string indicating the event that absorbs the remaining probability
  at `end` (i.e., its hazard at `end` is one minus the default hazard,
  so that termination at `end` is certain). Only used when `x` is of
  class `alife_multi`.

## Value

An object of the same class as `x` with the extrapolated lifetime
support.

## Details

The last observed time point is replaced: the extrapolation starts from
the hazard at the second-to-last observed time point. In a discrete
setting, a `geometric` extrapolation is a constant hazard rate, similar
to an expontential extrapolation in a continuous setting.
