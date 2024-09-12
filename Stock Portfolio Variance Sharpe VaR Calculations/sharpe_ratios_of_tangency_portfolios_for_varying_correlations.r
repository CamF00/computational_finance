# Load necessary library
library(quadprog)

# Input data
returns <- c(0.0310, 0.0950, 0.0660)  # Expected returns for BMW, Tesla, Volkswagen
sigmas <- c(0.1580, 0.2370, 0.1880)   # Standard deviations for BMW, Tesla, Volkswagen

# Define initial covariance matrix using standard deviations and given covariances
covariances <- matrix(c(
  0.1580^2, 0.0067, 0.0040,  # BMW-Tesla, BMW-Volkswagen
  0.0067, 0.2370^2, -0.0036, # Tesla-Volkswagen
  0.0040, -0.0036, 0.1880^2  # Variances for BMW, Tesla, Volkswagen
), nrow = 3, byrow = TRUE)

# Define the range of correlations between BMW and Tesla
correlations <- seq(-0.40, 0.80, by = 0.01)

# Set the risk-free rate
risk_free_rate <- 0.08  # 8%

# Initialize vector to store Sharpe Ratios of tangency portfolios
sharpe_ratios <- numeric(length(correlations))

# Function to compute the tangency portfolio weights and Sharpe ratio
compute_sharpe_ratio <- function(cov_matrix, returns, risk_free_rate) {
  # Excess returns
  excess_returns <- returns - risk_free_rate
  
  # Quadratic programming to maximize Sharpe ratio (tangency portfolio)
  Dmat <- 2 * cov_matrix
  dvec <- rep(0, length(returns))
  Amat <- cbind(excess_returns, diag(1, length(returns)))
  bvec <- c(1, rep(0, length(returns)))
  
  # Solve the quadratic programming problem
  qp_result <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  weights <- qp_result$solution
  
  # Compute expected return and standard deviation of tangency portfolio
  portfolio_return <- sum(weights * returns)
  portfolio_variance <- t(weights) %*% cov_matrix %*% weights
  portfolio_sd <- sqrt(portfolio_variance)
  
  # Sharpe ratio calculation
  sharpe_ratio <- (portfolio_return - risk_free_rate) / portfolio_sd
  
  return(sharpe_ratio)
}

# Loop over the range of correlations
for (i in seq_along(correlations)) {
  # Update covariance between BMW and Tesla
  cov_bmw_tesla <- correlations[i] * sigmas[1] * sigmas[2]
  
  # Update the covariance matrix with the new BMW-Tesla covariance
  cov_matrix <- covariances
  cov_matrix[1, 2] <- cov_bmw_tesla  # Update BMW-Tesla
  cov_matrix[2, 1] <- cov_bmw_tesla  # Symmetric matrix
  
  # Compute the Sharpe ratio for the tangency portfolio
  sharpe_ratios[i] <- compute_sharpe_ratio(cov_matrix, returns, risk_free_rate)
}

# Create a data frame for the results
results <- data.frame(Correlation_BMW_Tesla = correlations, Sharpe_Ratio = sharpe_ratios)

# Print results
print(results)
