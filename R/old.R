## Asymptotic variance for logit-transformed confidence intervals
## 
## This function calculates the estimated asymptotic variance of the
## logit-transformed hazard rate estimates.  It is used to construct the
## asymptotic confidence intervals for the hazard rate estimates.
## The logit-transformation ensures the estimated confidence intervals are
## bounded between 0 and 1 (Lautier et al., 2026a).
##  
## @param risk_set The estimated risk set, \eqn{\hat{U}} (see [u_hat()]).
## @param fh The estimated, unadjusted event probabilities, \eqn{\hat{f}}
## (see [f_hat()]).
## @param n The sample size.
## 
## @note The asymptotic, logit-transformed variance estimator simplifies to its
## equivalent in Lautier et al. (2023) if there is only one event type (i.e.,
## no competing risks).  If there is both only one event type and no
## right-censoring, then the asymptotic, logit-transformed variance estimator
## further simplifies to its equivalent in Lautier et al. (2026b).
## 
## @return a positive scalar
## 
## @references
## {Lautier, J. P., Pozdnyakov, V., Yan, J. (2023). Pricing time-to-event
## contingent cash-flows: A discrete-time survival analysis approach.
## *Insurance: Mathematics and Economics*, 110, 53-71.
## \url{https://doi.org/10.1016/j.insmatheco.2023.02.003} }
## 
## {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026a). On the convergence of
## credit risk in current consumer automobile loans.  *Journal of the Royal
## Statistical Society Series A: Statistics in Society*, 189 (1), 196-221.
## \url{https://doi.org/10.1093/jrsssa/qnae137}}
## 
## {Lautier, J. P., Pozdnyakov, V., Yan, J. (2026b). Estimating a discrete
## distribution subject to random left-truncation with an application to
## structured finance. *Econometrics and Statistics*, 37, 174-198.
## \url{https://doi.org/10.1016/j.ecosta.2023.05.005} }
## 
## @keywords internal
##

##  var_hat <- function(risk_set, fh, n) {
##   uh <- risk_set
##   lfh <- log(fh)
##   luh <- log(uh)
##   exp(log(uh - fh) -  log(n) - luh - lfh)
## }
