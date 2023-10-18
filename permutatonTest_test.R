#
#   This code is made to test the permutationTest-function
#   18-10-2023
#
#   Runs 1000 permutation tests on each 0.05 step of correlation -1 to 1. 
#   Measures the rate of rejected H_0 hypotheses for each level, where the H_0
#   hypothesis states that there is no correlation.
#
rm(list = ls())
set.seed(100)   # Should be set to 100 for reproducability of results
library(MASS)
library(magrittr)
library(ggplot2)

# Parameters
NUM_PERM <- 100   # Number of permutations used in permutation test
K        <- 1000  # Number of tests for each correlation level
N        <- 1000  # Number of datapoints that we generate for each test

# Run a permutation test between two data sets using Perasons
# Correlation Coefficient (PCC). Returns P-value for the test
permutationTest <- function(A, B) {
  # Firstly calculate the correlation coefficient from original data
  obs_cor <- cor(A, B)
  
  # Calculate the PCC NUM_PERM times
  pTest_cors <- NUM_PERM %>% numeric
  for(i in 1:NUM_PERM) {
    permuted_B <- B %>% sample
    pTest_cors[i] <- cor(A, permuted_B)
  }
  
  # Return the rate of which we had larger PCC than the observed
  # PCC on the original data
  return(mean(abs(pTest_cors) >= abs(obs_cor)))
}

# Generate two correlated data sets based on the wanted correlation level
generateData <- function(correlation) {
  # First generate the covariance matrix, then generate the correlated data
  cov_matrix <- matrix(c(1, correlation, correlation, 1), nrow = 2)
  return(mvrnorm(N, mu = c(0, 0), Sigma = cov_matrix))
}

########## TEST ##########
steps <- seq(-1, 1, by = 0.05) # Generate a vector from -1 to 1 of 0.05 steps

# Perform the test
rates <- steps %>% length %>% numeric
for(i in 1:length(steps)) {
  correlations <- K %>% numeric
  print(paste("Running correlation level:", steps[i]))
  for(k in 1:K) {
    data <- generateData(steps[i])
    correlations[k] <- permutationTest(data[, 1], data[, 2])
  }
  rates[i] <- mean(correlations <= 0.05)
}

# Visualize the results
data <- data.frame(steps, rates)
ggplot(data, aes(x = steps, y = rates)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ylim(0, 1) +
  labs(title = "Test of: permutationTest",
       x = "Correlation level (PCC)",
       y = "Rate of rejected H_0-Hypotheses")
