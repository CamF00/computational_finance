# Load necessary library
library(quadprog)

# Input data
returns <- c(0.0310, 0.0950, 0.0660)  # Expected returns for BMW, Tesla, Volkswagen
sigmas <- c(0.1580, 0.2370, 0.1880)   # Standard deviations for BMW, Tesla, Volkswagen
covariances <- matrix(c(
  0.1580^2, 0.0067, 0.0040,  # Covariances: BMW-Tesla, BMW-Volkswagen
  0.0067, 0.2370^2, -0.0036, # Covariances: Tesla-Volkswagen
  0.0040, -0.0036, 0.1880^2  # Variances: BMW, Tesla, Volkswagen
), nrow = 3, byrow = TRUE)

# (i) Global Minimum Variance Portfolio (GMVP)
Dmat <- covariances
dvec <- rep(0, 3)  # No linear term for the quadratic programming problem
Amat <- cbind(1, diag(3))  # Constraints: weights sum to 1, no short-selling (weights >= 0)
bvec <- c(1, rep(0, 3))  # Weights sum to 1

# Solve for weights
gmvp_solution <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
gmvp_weights <- gmvp_solution$solution

# Display GMVP weights
cat("GMVP Weights for BMW, Tesla, Volkswagen:\n")
cat(sprintf("BMW: %.4f, Tesla: %.4f, Volkswagen: %.4f\n", gmvp_weights[1], gmvp_weights[2], gmvp_weights[3]))

# (ii) Sharpe Ratio of the GMVP
risk_free_rate <- 0.025  # Risk-free rate (2.5%)
gmvp_return <- sum(gmvp_weights * returns)  # Expected return of GMVP
gmvp_variance <- t(gmvp_weights) %*% covariances %*% gmvp_weights  # Variance of GMVP
gmvp_sd <- sqrt(gmvp_variance)  # Standard deviation of GMVP

sharpe_ratio_gmvp <- (gmvp_return - risk_free_rate) / gmvp_sd  # Sharpe ratio calculation

# Display Sharpe Ratio
cat(sprintf("Sharpe Ratio of GMVP: %.4f\n", sharpe_ratio_gmvp))

# (iii) Annual Value-At-Risk (VaR) of the GMVP at the 99% confidence level
investment_amount <- 10000000  # Amount invested in GMVP ($10,000,000)
z_score_99 <- qnorm(0.99)  # Z-score for 99% confidence level

# VaR calculation
annual_var_gmvp <- investment_amount * gmvp_sd * z_score_99

# Display Value-At-Risk
cat(sprintf("Annual Value-At-Risk (VaR) of the GMVP at 99%% confidence level: $%.2f\n", annual_var_gmvp))
