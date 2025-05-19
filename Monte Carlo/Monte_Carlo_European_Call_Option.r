# Using the Monte Carlo method, write an R script that calculates the value of a European call option
# and it's standard error on a stock which is currently traded at a price of $120. The option has an
# exercise price of $110 and will expire in 6 months. The stock price volatility is 12.5% and the 
# risk-free rate is 1.5%. Run the simulation 10,000 times and state all the assumptions you make in
# the script.

# Clear the workspace
Rm(list = ls(all = TRUE))

# Input the data
S0 <- 120
K <- 110
N <- 10000
T <- (6/12)
rf <- 0.015
sigma <- 0.125

# Pre-compute constants
B <- 1 / exp(rf * T)
ST <- rep(0, N)
CT <- rep(0, N)
epsilon <- rnorm(N, 0, 1)

# Geometric Brownian Motion => ST = S0 * exp((Rf - (Sigma^2/2) * T) + (sigma * epsilon * sqrt(T)
ST <- S0 * exp((rf - (sigma^2/2)) * T) + (sigma * epsilon * sqrt(T))

# Calculating the call option payoff
for(Sim.idx in 1:N) {
  CT[Sim.idx] <- max(0, ST[Sim.idx] - K)
}

# Calculating the call option value and the standard error of the option
Expected.CT <- mean(CT)
Call.val <- B * Expected.CT
Call.SE <- sd(CT) / sqrt(N)
                                          
print(Call.val)
print(Call.SE)
                                         
