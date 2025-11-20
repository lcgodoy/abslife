# abslife 0.0.94

* `time_to_event` becomes lifetime

* Logo included in the package website and README file.

* First draft for a vignette.

* Two new datasets: `aart` (for left-truncation) and `mbalt` (left-truncation +
  right-censoring).

* `event_indicator` is no longer an argument of `estimate_hazard()`.

# abslife 0.0.93

* `ggplot2` included as a suggested package

* fixed "check" for number of events

* `estimate_hazard` broken into smaller functions to make it easier to mantain.

* Included a `summary` function for the `alife` objects

* Extended plot function to deal with multiple event types

* `rlang` becomes a dependency :(

* `autoplot` method for making graphs with `ggplot2`

# abslife 0.0.92

* Fixed the competing risks approach.

# abslife 0.0.91

* `event_type` parameter in the `estimate_hazard` function now deals with
  competing risks.

* `single_t_hazard` internal function created. It is meant to make the code
  easier to debug by expliting the big `estimate_hazard` function into
  self-contained chunks.

* `ci_level` now is a parameter of the `estimate_hazard` function.

* First draft for the `estimate_hazard` function. So far, we do not have
  competing risks implemented.
  
* `calc_tp` helper calculates $\Delta$ and $m$ based on left-truncation and
  time-to-event variables and outputs a sequence ranging from $\Delta + 1$ to
  $\Delta + m$.
  
* Create a class `alife` to enable a `plot` method for the `estimate_hazard`
  function output.
  
* Function to carry hazard estimates forward when a "zero hazard" is observed.

# abslife 0.0.9

* Only the dataset `aloans` has been included so far.
