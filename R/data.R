##' Consumer automobile loans dataset
##'
##' A dataset containing the performance records of 58,118 consumer automobile
##' loans with original terms of 72 to 73 months. The data was obtained from
##' four publicly traded asset-backed securities (ABS) bonds. This dataset is
##' subject to left-truncation (loans must survive long enough to be included in
##' the ABS trust) and right-censoring (the trust may be unwound before all
##' loans terminate), and it features competing risks of default and prepayment.
##'
##' @format ## `aloans`
##' A data frame with 58,118 rows and 7 columns:
##' \describe{
##' \item{risk_cat}{Credit risk band based on the loan's Annual Percentage Rate
##' (APR). Categories include "super_prime" (0-5%), "prime" (5-10%), "near_prime"
##' (10-15%), "subprime" (15-20%), and "deep_subprime" (20%+).}
##' \item{Z}{Time-to-event, representing the loan termination age in months.}
##' \item{Y}{Left-truncation time, representing the loan age in months
##'   when the ABS trust began making payments to investors.}
##' \item{C}{Right-censoring indicator (1 = right-censored, 0 = exact
##' termination observed).}
##' \item{D}{Default indicator for competing risks (1 = default, 0 = prepayment).}
##' \item{bond}{Categorical variable specifying the source ABS bond: CARMX
##' (CarMax Auto Owner Trust 2017-2), AART (Ally Auto Receivables Trust 2017-3),
##' SDART (Santander Drive Auto Receivables Trust 2017-2), or DRIVE (Drive Auto
##' Receivables Trust 2017-1).}
##' }
##'
##' @source Data was originally compiled from the SEC's Electronic Data
##'   Gathering, Analysis, and Retrieval (EDGAR) system. The replication data
##'   repository is available at
##'   <https://github.com/jackson-lautier/credit-risk-convergence/>.
##' 
##' @references Lautier, J. P., Pozdnyakov, V., & Yan, J. (2024). On the
##'   convergence of credit risk in current consumer automobile
##'   loans. \emph{Journal of the Royal Statistical Society Series A: Statistics
##'   in Society}, qnae137. \doi{10.1093/jrsssa/qnae137}.
"aloans"

##' Consumer automobile loans from Ally Auto Receivables Trust
##'
##' A dataset containing time-to-event and left-truncation times for consumer
##' automobile loans from the Ally Auto Receivables Trust (AART). The data were
##' compiled from the SEC's EDGAR system.
##'
##' @format ## `aart`
##' A data frame with 1,553 rows and 2 columns:
##' \describe{
##'   \item{\code{Xi}}{Time-to-event, representing the loan termination age 
##'   in months.}
##'   \item{\code{Yi}}{Left-truncation time, representing the loan age in 
##'   months when the ABS trust began making payments to investors.}
##' }
##'
##' @source Data was compiled from the SEC's Electronic Data Gathering, 
##' Analysis, and Retrieval (EDGAR) system. The replication data repository 
##' is available at 
##' <https://github.com/jackson-lautier/consumer-auto-abs-parametric>.
##' 
##' @references
##' Lautier, J. P., Pozdnyakov, V., & Yan, J. (2025). Estimating the 
##' time-to-event distribution for loan-level data within a consumer auto loan 
##' asset-backed security. \emph{The Annals of Applied Statistics}. 
##' \doi{10.1214/25-AOAS2103}.
"aart"

##' Consumer automobile leases from Mercedes-Benz Auto Lease Trust 2017-A
##'
##' A dataset containing performance records of 47,315 consumer automobile
##' leases with an original term of 36 months from the Mercedes-Benz Auto Lease
##' Trust (MBALT) 2017-A.
##'
##' @format ## `mbalt`
##' A data frame with 47,315 rows and 3 columns:
##' \describe{
##'   \item{\code{Zi}}{Time-to-event, representing the observed lease 
##'   termination age in months.}
##'   \item{\code{Yi}}{Left-truncation time, representing the lease age in 
##'   months when the ABS trust began making payments to investors.}
##'   \item{\code{Di}}{Event indicator (1 = exact termination observed, 
##'   0 = right-censored).}
##' }
##'
##' @source Data was compiled from the SEC's Electronic Data Gathering, 
##' Analysis, and Retrieval (EDGAR) system.
##' 
##' @references Lautier, J. P., Pozdnyakov, V., & Yan, J. (2023). Estimating a
##'   discrete distribution subject to random left-truncation with an
##'   application to structured finance. \emph{Econometrics and Statistics}.
##'   \doi{10.1016/j.ecosta.2023.05.005}.
##'
##'   Lautier, J. P., Pozdnyakov, V., & Yan, J. (2023). Pricing time-to-event
##'   contingent cash flows: A discrete-time survival analysis
##'   approach. \emph{Insurance: Mathematics and Economics}, 110,
##'   53-71. \doi{10.1016/j.insmatheco.2023.02.003}.
"mbalt"
