# abslife 0.0.91

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
