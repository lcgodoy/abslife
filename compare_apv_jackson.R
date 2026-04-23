# compare_apv_jackson.R
# This script is a refactored version of resources_new_functions/ime_apv_jackson.R
# using the new functions from the 'abslife' package.

library(abslife)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(MASS)

# Note: The fitter logic is still used to prepare the survival data, 
# delay probabilities, and depreciation estimates.
source('resources_new_functions/m17_fitter.R')

# Mocking the data directory if needed, but here we assume the file structure from the original
# Or we can adjust it to point to the data/ directory in the package
# data_directory <- 'data/' # If running from project root

# For comparison, we will focus on the loop in Section 6.2 / TABLE 2 of the original script.
lease_term <- 36
o <- 6 # Observation window

# 1. Get inputs from the fitter
inputs <- mbalt17_fitter(o, lease_term)
est_dist_raw <- inputs[[1]] # Age, lam_hat, Var_hat
delay_prob_matrix <- inputs[[2]]
depreciation_matrix_raw <- inputs[[3]]
m36_data <- inputs[[4]]

# 2. Convert raw fitter output to abslife 'acdf' object
# We manually compute cdf and density to match the fitter's hazards exactly
hazards <- est_dist_raw$lam_hat
cdf_vals <- 1 - cumprod(1 - hazards)
density_vals <- cdf_vals - c(0, cdf_vals[-length(cdf_vals)])

acdf_obj <- new_acdf(data.frame(
  lifetime = est_dist_raw$Age,
  cdf = cdf_vals,
  density = density_vals,
  risk_set = 1 # dummy for validation
))

# 3. Prepare matrices and variables as in the original script
# Ensure delay matrix has "0" column
delay_prob_matrix[,"0"] <- ifelse(is.na(delay_prob_matrix[,"0"]), 1, delay_prob_matrix[,"0"])
delay_prob_matrix <- ifelse(is.na(delay_prob_matrix), 0, delay_prob_matrix)

# depreciation_matrix: rows are Age, col1=EZx, col2=VarZx
depreciation_matrix <- matrix(c(depreciation_matrix_raw$EZx, depreciation_matrix_raw$VarZx), ncol=2)
rownames(depreciation_matrix) <- depreciation_matrix_raw$Age
depreciation_matrix <- ifelse(is.na(depreciation_matrix), 0, depreciation_matrix)
# Patching as in original
last_age <- as.character(max(depreciation_matrix_raw$Age))
depreciation_matrix[last_age, 1] <- ifelse(depreciation_matrix[last_age, 1] == 0, 1, depreciation_matrix[last_age, 1])
depreciation_matrix[,1] <- ifelse(depreciation_matrix[,1] < 0.10, 1, depreciation_matrix[,1])

# Discount factors (dummy/norm_disc from original yc.csv if available)
# Since yc.csv is not provided in resources, we'll use a dummy vector of 1s or mock norm_disc
# Looking at original: yc_loop = list(rep(1,30), yc$norm_disc, yc$inv_disc)
# We'll use rep(1, 40) for this comparison script
discount_factors <- rep(1, 40)

# 4. Select a subset of active leases to calculate APV for
m_paying <- data.frame(
  elapsed_time = m36_data$Xc[m36_data$C == 0],
  payment = m36_data$contract_pmt[m36_data$C == 0],
  residual_value = m36_data$contractResidualValue[m36_data$C == 0]
)

# 5. Calculation using Package Functions
payment_cap <- 3
max_delay <- max(as.numeric(colnames(delay_prob_matrix)))
max_lifetime <- max(acdf_obj$lifetime)

# Calculate APV and Variance for the first 10 paying leases
cat("Calculating APV for the first 10 active leases using package functions...\n")
results <- data.frame(Lease = 1:10, APV = NA, Var = NA)

for(i in 1:10) {
  row <- m_paying[i, ]
  
  # Package call for APV
  results$APV[i] <- calc_apv(
    acdf_obj, 
    payment = row$payment,
    residual_value = row$residual_value,
    elapsed_time = row$elapsed_time,
    max_lifetime = max_lifetime,
    max_delay = max_delay,
    payment_cap = payment_cap,
    discount_factors = discount_factors,
    delay_prob_matrix = delay_prob_matrix,
    depreciation_matrix = depreciation_matrix
  )
  
  # Package call for Variance
  results$Var[i] <- calc_apv_var(
    acdf_obj, 
    payment = row$payment,
    residual_value = row$residual_value,
    elapsed_time = row$elapsed_time,
    max_lifetime = max_lifetime,
    max_delay = max_delay,
    payment_cap = payment_cap,
    discount_factors = discount_factors,
    delay_prob_matrix = delay_prob_matrix,
    depreciation_matrix = depreciation_matrix
  )
}

print(results)

cat("\nSummary of Package implementation vs Original Logic:\n")
cat("1. Loops over k and l are now encapsulated in calc_apv().\n")
cat("2. probX, PD_star, Wi_star are now internal package helpers.\n")
cat("3. S3 dispatch allows passing 'acdf' or 'alife' objects directly.\n")
cat("4. Arguments are explicitly named (snake_case) for clarity.\n")
