# The following table shows the annualised daily returns on three stocks between January 2, 2014
# and December 31, 2014. Use the information below to answer the questions.

# Stock i ⏐ Expected return i ⏐ Volatility i ⏐ Pair(i, j) ⏐ Asset Covariance
# Accenture ⏐ 0.0310 ⏐ 0.1580 ⏐ (Accenture, BP) ⏐ 0.0067
# BP ⏐ 0.0950 ⏐ 0.2370 ⏐ (BP, Costco) ⏐ -0.0036
# Costco ⏐ 0.0660 ⏐ 0.1880 ⏐ (Accenture, Costco ⏐ 0.0040

# a) Using package quadprog, write an R script which does the following: (i) identify a long-only
# portfolio that has the same expected return as BP (i.e., specify the fractions of wealth
# invested in each of the asset); (ii) calculate the portfolio's standard deviation.

# Clean the workspace
rm(list = ls(all = TRUE))

# Load the necessary library
library(quadprog)

# Input the data
r.f <- 0.015
mu.A <- 0.0310
mu.B <- 0.0950
mu.C <- 0.660
sigma.A <- 0.1580
sigma.B <- 0.2370
sigma.C <- 0.1880
sigma.AB <- 0.0067
sigma.BC <- -0.0036
sigma.AC <- 0.0040

# Assigning values to names.
asset.names <- c("Accenture", "BP", "Costco")
mu.vec <- c(mu.A, mu.B, mu.C)
names(mu.vec) <- asset.names

Sigma.mat <- matrix(c(sigma.A^2, sigma.AB, sigma.AC,
                      sigma.AB, sigma.B^2, sigma.BC,
                      sigma.AC, sigma.BC, sigma.C^2),
                    nrow = 3, ncol = 3)

dimnames(Sigma.mat) <- list(asset.names, asset.names)

# Setting restriction matrices.
D.mat <- 2 * Sigma.mat
D.vec <- rep(0, 3)
A.mat <- cbind(mu.vec, rep(1, 3), diag(3))
B.vec <- c(mu.B, 1, rep(0, 3))

output <- solve.QP(Dmat = D.mat, dvec = D.vec, Amat = A.mat, dvec = B.vec, meq = 2)

# i) 
x.star.vec <- output$solution
print(x.star.vec)

# ii) Calculating the portfolio's standard deviation
sigma.P.star <- sqrt(output$value)
print(sigma.P.star)

# b) Write an R script which calculates the Value-at-Risk (VaR) of the long-only portfolio,
# constructed in part (a), at the 1%, 5% and 10% significance levels over the next 6 months.
# The initial investment, W0 is assumed to be equal to $15,000,000.

W0 <- 15000000

VaR.01 <- sqrt(0.5) * W0 * (mu.B + (sigma.P.star * qnorm(0.01)))
VaR.05 <- sqrt(0.5) * W0 * (mu.B + (sigma.P.star * qnorm(0.05)))
VaR.10 <- sqrt(0.5) * W0 * (mu.B + (sigma.P.star * qnorm(0.10)))

print(VaR.01)
print(VaR.05)
print(VaR.10)
