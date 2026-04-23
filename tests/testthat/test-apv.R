library(testthat)
library(abslife)

test_that("APV calculation works with dummy data", {
  # Create a simple acdf object
  # Time points 1 to 5
  # f(t) = 0.2 for all t in {1, 2, 3, 4, 5}
  # F(t) = 0.2*t
  df <- data.frame(
    lifetime = 1:5,
    cdf = c(0.2, 0.4, 0.6, 0.8, 1.0),
    density = rep(0.2, 5),
    risk_set = rep(1, 5) # dummy
  )
  x <- new_acdf(df)
  
  # Discount factors (no discounting for simplicity)
  discount_factors <- rep(1, 10)
  
  # Delay probability matrix (no delay)
  delay_prob_matrix <- matrix(0, nrow = 5, ncol = 6)
  rownames(delay_prob_matrix) <- 1:5
  colnames(delay_prob_matrix) <- 0:5
  delay_prob_matrix[, "0"] <- 1
  
  # Depreciation matrix (no depreciation, mult = 1, var = 0)
  depreciation_matrix <- matrix(0, nrow = 5, ncol = 2)
  rownames(depreciation_matrix) <- 1:5
  depreciation_matrix[, 1] <- 1
  
  # elapsed_time = 1, max_lifetime = 5
  # payment = 100, residual_value = 1000, cap = 10
  # Since f(t) = 0.2, and elapsed_time = 1, P(X=t|X>=1) = 0.2 / (1-0) = 0.2
  # For each t in {1,2,3,4,5}:
  # PV = (payment * (t - 0 - 1 + 1)) + (residual * 1) = 100*t + 1000
  # Expected PV = sum_{t=1}^5 0.2 * (100*t + 1000)
  # = 0.2 * ( (100*1+1000) + (100*2+1000) + (100*3+1000) + (100*4+1000) + (100*5+1000) )
  # = 0.2 * ( 1100 + 1200 + 1300 + 1400 + 1500 )
  # = 0.2 * ( 6500 ) = 1300
  
  apv <- calc_apv(x, payment = 100, residual_value = 1000, elapsed_time = 1, 
                  max_lifetime = 5, max_delay = 5, payment_cap = 10, 
                  discount_factors = discount_factors, 
                  delay_prob_matrix = delay_prob_matrix, 
                  depreciation_matrix = depreciation_matrix)
  
  expect_equal(apv, 1300)
})

test_that("APV variance calculation works with dummy data", {
  df <- data.frame(
    lifetime = 1:5,
    cdf = c(0.2, 0.4, 0.6, 0.8, 1.0),
    density = rep(0.2, 5),
    risk_set = rep(1, 5)
  )
  x <- new_acdf(df)
  discount_factors <- rep(1, 10)
  delay_prob_matrix <- matrix(0, nrow = 5, ncol = 6)
  rownames(delay_prob_matrix) <- 1:5
  colnames(delay_prob_matrix) <- 0:5
  delay_prob_matrix[, "0"] <- 1
  depreciation_matrix <- matrix(0, nrow = 5, ncol = 2)
  rownames(depreciation_matrix) <- 1:5
  depreciation_matrix[, 1] <- 1
  
  # Expected second moment = sum_{t=1}^5 0.2 * (100*t + 1000)^2
  # = 0.2 * ( 1100^2 + 1200^2 + 1300^2 + 1400^2 + 1500^2 )
  # = 0.2 * ( 1,210,000 + 1,440,000 + 1,690,000 + 1,960,000 + 2,250,000 )
  # = 0.2 * ( 8,550,000 ) = 1,710,000
  # Variance = 1,710,000 - 1300^2 = 1,710,000 - 1,690,000 = 20,000
  
  v <- calc_apv_var(x, payment = 100, residual_value = 1000, elapsed_time = 1, 
                    max_lifetime = 5, max_delay = 5, payment_cap = 10, 
                    discount_factors = discount_factors, 
                    delay_prob_matrix = delay_prob_matrix, 
                    depreciation_matrix = depreciation_matrix)
  
  expect_equal(v, 20000)
})

test_that("APV calculation works with multi-event data", {
  df <- data.frame(
    event_type = rep(c("A", "B"), each = 5),
    lifetime = rep(1:5, 2),
    cdf = rep(c(0.2, 0.4, 0.6, 0.8, 1.0), 2),
    density = rep(0.2, 10),
    risk_set = rep(1, 10)
  )
  x <- new_acdf(df)
  
  discount_factors <- rep(1, 10)
  delay_prob_matrix <- matrix(0, nrow = 5, ncol = 6)
  rownames(delay_prob_matrix) <- 1:5
  colnames(delay_prob_matrix) <- 0:5
  delay_prob_matrix[, "0"] <- 1
  depreciation_matrix <- matrix(0, nrow = 5, ncol = 2)
  rownames(depreciation_matrix) <- 1:5
  depreciation_matrix[, 1] <- 1
  
  apvs <- calc_apv(x, payment = 100, residual_value = 1000, elapsed_time = 1, 
                   max_lifetime = 5, max_delay = 5, payment_cap = 10, 
                   discount_factors = discount_factors, 
                   delay_prob_matrix = delay_prob_matrix, 
                   depreciation_matrix = depreciation_matrix)
  
  expect_type(apvs, "list")
  expect_equal(apvs$A, 1300)
  expect_equal(apvs$B, 1300)
})
