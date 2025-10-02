# abslife 0.0.91

* First draft for the `estimate_hazard` function. So far, we do not have
  competing risks implemented.
  
* `calc_tp` helper calculates $\delta$ and $m$ based on left-truncation and
  time-to-event variables and outputs a sequence ranging from $\delta + 1$ to
  $\delta + m$.
  
* Create a class `alife` to enable a `plot` method for the `estimate_hazard`
  function output.
  
* Function to carry hazard estimates forward when a "zero hazard" is observed.

# abslife 0.0.9

* Only the dataset `aloans` has been included so far.
