#' @title Internal helper for probability of termination
#' @param x An `acdf` object.
#' @param lifetime Time of termination.
#' @param elapsed_time Current age.
#' @keywords internal
prob_termination <- function(x, lifetime, elapsed_time) {
  # Probability of termination at 'lifetime' given survival to 'elapsed_time'
  # P(X = s | X >= elapsed_time) = f(s) / (1 - F(elapsed_time - 1))
  
  f_lifetime <- x$density[x$lifetime == lifetime]
  if (length(f_lifetime) == 0) return(0)
  
  # F(elapsed_time - 1)
  # Find the largest time point in x$lifetime that is < elapsed_time
  times_less <- x$lifetime[x$lifetime < elapsed_time]
  if (length(times_less) == 0) {
    F_minus <- 0
  } else {
    F_minus <- x$cdf[x$lifetime == max(times_less)]
  }
  
  return(f_lifetime / (1 - F_minus))
}

#' @title Internal helper for residual present value
#' @param residual_value Contractual residual value.
#' @param lifetime Time of termination.
#' @param elapsed_time Current age.
#' @param discount_factors Vector of discount factors.
#' @keywords internal
residual_pv <- function(residual_value, lifetime, elapsed_time, discount_factors) {
  # Paper notation: Ri * nu[X_i - x_e + 1]
  idx <- lifetime - elapsed_time + 1
  if (idx > length(discount_factors)) return(0)
  return(residual_value * discount_factors[idx])
}

#' @title Internal helper for payment present value
#' @param payment Monthly payment.
#' @param lifetime Time of termination.
#' @param delay Delay in payment.
#' @param elapsed_time Current age.
#' @param payment_cap Cap on payments.
#' @param discount_factors Vector of discount factors.
#' @keywords internal
payment_pv <- function(payment, lifetime, delay, elapsed_time, payment_cap, discount_factors) {
  # Paper notation: Wi_star = sum( pmt * nu[1:stop] ) where stop = min(cap, X_i - D_Xi - x_e + 1)
  idx_max <- lifetime - delay - elapsed_time + 1
  if (idx_max <= 0) return(0)
  
  stop_idx <- min(payment_cap, idx_max)
  if (stop_idx > length(discount_factors)) stop_idx <- length(discount_factors)
  
  return(sum(payment * discount_factors[1:stop_idx]))
}

#' @title Internal helper for probability of delay
#' @param delay Length of delay.
#' @param lifetime Time of termination.
#' @param elapsed_time Current age.
#' @param max_delay Maximum possible delay.
#' @param delay_prob_matrix Delay probability matrix.
#' @keywords internal
prob_delay <- function(delay, lifetime, elapsed_time, max_delay, delay_prob_matrix) {
  # Paper notation: PD_star
  row_name <- as.character(lifetime)
  col_name <- as.character(delay)
  
  if (!(row_name %in% rownames(delay_prob_matrix))) return(0)
  if (!(col_name %in% colnames(delay_prob_matrix))) return(0)
  
  base <- delay_prob_matrix[row_name, col_name]
  
  # if((delay == (lifetime - elapsed_time + 1)) & (delay < max_delay))
  if ((delay == (lifetime - elapsed_time + 1)) && (delay < max_delay)) {
    # extra delay probabilities accumulated at the boundary
    extra_delay_range <- (lifetime - elapsed_time + 2):max_delay
    extra_delay_names <- as.character(extra_delay_range)
    extra_delay_names <- extra_delay_names[extra_delay_names %in% colnames(delay_prob_matrix)]
    
    extra <- sum(delay_prob_matrix[row_name, extra_delay_names])
    return(base + extra)
  } else {
    return(base)
  }
}

#' @title Calculate Expected Actuarial Present Value
#' 
#' @description Computes the Expected Actuarial Present Value (APV) of future cash flows
#' given a survival model (`acdf` or `alife` object).
#' 
#' @param x An object of class `acdf` or `alife`.
#' @param payment Monthly payment amount.
#' @param residual_value Contractual residual value.
#' @param elapsed_time Current age (time elapsed) of the lease.
#' @param max_lifetime Maximum time to consider (time horizon).
#' @param max_delay Maximum possible payment delay.
#' @param payment_cap Cap on the number of payments to be made.
#' @param discount_factors Numeric vector of monthly discount factors.
#' @param delay_prob_matrix Matrix where rows are termination times and columns are delay lengths.
#' @param depreciation_matrix Matrix with rows as termination times, first column is expectation and second column is variance of depreciation.
#' @param ... Additional arguments.
#' @export
calc_apv.acdf <- function(x, payment, residual_value, elapsed_time, max_lifetime, 
                          max_delay, payment_cap, discount_factors, 
                          delay_prob_matrix, depreciation_matrix, ...) {
  
  expected_val <- 0
  lifetimes <- elapsed_time:max_lifetime
  
  for (l in lifetimes) {
    p_term <- prob_termination(x, l, elapsed_time)
    if (p_term == 0) next
    
    # Expected value given termination at l
    expected_delay_val <- 0
    max_k <- min(max_delay, l - elapsed_time + 1)
    if (max_k < 0) max_k <- 0
    
    for (k in 0:max_k) {
      p_delay <- prob_delay(k, l, elapsed_time, max_delay, delay_prob_matrix)
      w_star <- payment_pv(payment, l, k, elapsed_time, payment_cap, discount_factors)
      expected_delay_val <- expected_delay_val + w_star * p_delay
    }
    
    # Residual part
    res_part <- 0
    if ((l - elapsed_time + 1) <= payment_cap) {
      deprec_mult <- depreciation_matrix[as.character(l), 1]
      res_part <- residual_pv(residual_value, l, elapsed_time, discount_factors) * deprec_mult
    }
    
    expected_val <- expected_val + p_term * (expected_delay_val + res_part)
  }
  
  return(unname(expected_val))
}

#' @export
calc_apv.alife <- function(x, ...) {
  x_cdf <- calc_cdf(x)
  calc_apv(x_cdf, ...)
}

#' @export
calc_apv.acdf_multi <- function(x, ...) {
  df_list <- split(x, x$event_type)
  out <- lapply(df_list, function(df) {
    class(df) <- c("acdf", "data.frame")
    calc_apv(df, ...)
  })
  return(out)
}

#' @title Calculate Variance of Actuarial Present Value
#' 
#' @description Computes the variance of the Actuarial Present Value (APV) of future 
#' cash flows.
#' 
#' @inheritParams calc_apv.acdf
#' @export
calc_apv_var.acdf <- function(x, payment, residual_value, elapsed_time, max_lifetime, 
                              max_delay, payment_cap, discount_factors, 
                              delay_prob_matrix, depreciation_matrix, ...) {
  
  second_moment <- 0
  lifetimes <- elapsed_time:max_lifetime
  
  for (l in lifetimes) {
    p_term <- prob_termination(x, l, elapsed_time)
    if (p_term == 0) next
    
    # Second moment given termination at l
    ed2 <- 0
    ed <- 0
    max_k <- min(max_delay, l - elapsed_time + 1)
    if (max_k < 0) max_k <- 0
    
    for (k in 0:max_k) {
      p_delay <- prob_delay(k, l, elapsed_time, max_delay, delay_prob_matrix)
      w_star <- payment_pv(payment, l, k, elapsed_time, payment_cap, discount_factors)
      ed2 <- ed2 + (w_star^2) * p_delay
      ed <- ed + w_star * p_delay
    }
    
    # Residual part
    val <- ed2
    if ((l - elapsed_time + 1) <= payment_cap) {
      ri <- residual_pv(residual_value, l, elapsed_time, discount_factors)
      deprec_mu <- depreciation_matrix[as.character(l), 1]
      deprec_var <- depreciation_matrix[as.character(l), 2]
      
      # Term 2: 2 * Ri * deprec_mu * ed
      val <- val + 2 * ri * deprec_mu * ed
      
      # Term 3: (Ri^2) * (deprec_var + deprec_mu^2)
      val <- val + (ri^2) * (deprec_var + deprec_mu^2)
    }
    
    second_moment <- second_moment + p_term * val
  }
  
  apv <- calc_apv.acdf(x, payment, residual_value, elapsed_time, max_lifetime, 
                       max_delay, payment_cap, discount_factors, 
                       delay_prob_matrix, depreciation_matrix, ...)
  
  return(unname(second_moment - apv^2))
}

#' @export
calc_apv_var.alife <- function(x, ...) {
  x_cdf <- calc_cdf(x)
  calc_apv_var(x_cdf, ...)
}

#' @export
calc_apv_var.acdf_multi <- function(x, ...) {
  df_list <- split(x, x$event_type)
  out <- lapply(df_list, function(df) {
    class(df) <- c("acdf", "data.frame")
    calc_apv_var(df, ...)
  })
  return(out)
}
