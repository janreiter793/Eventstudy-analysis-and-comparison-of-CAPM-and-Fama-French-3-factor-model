################################################################################
#                                                                              #
#   The purpose of this code is to test the permutationTest-function           #
#   19-10-2023                                                                 #
#                                                                              #
#   Runs 1000 permutation tests on each 0.05 step of correlation -1 to 1.      #
#   Measures the rate of rejected H_0 hypotheses for each level, where the H_0 #
#   hypothesis states that there is no correlation. The permutation tests can  #
#   be run for Pearson's, Spearman's, and Kendall's correlation coefficient,   #
#   and it is tested for all three cases.                                      #
#                                                                              #
#   There are three choices of functions for generating data:                  #
#     1) generateData1 : generates two correlated normally distributed         #
#                        data sets.                                            #
#     2) generateData2 : generates two correlated data sets. One normally      #
#                        distributed and one gamma distributed                 #
#     3) generateData3 : generates two correlated data sets. One poisson       #
#                        distributed and one gamma distributed.                #
#                                                                              #
#   Choice of data generating function can be edited in line 94.               #
#                                                                              #
################################################################################
rm(list = ls())
set.seed(100)   # Should be set to 100 for reproducability of results
library(MASS)
library(magrittr)
library(ggplot2)
library(latex2exp)

# Parameters
NUM_PERM <- 100    # Number of permutations used in permutation test
K        <- 1000   # Number of tests for each correlation level
N        <- 100    # Number of datapoints that we generate for each test

# Run a permutation test between two data sets using either Pearson's, 
# Spearman's or Kendall's correlation coefficient. Returns P-value for the test.
permutationTest <- function(A, B, method) {
  # Firstly calculate the correlation coefficient from original data
  obs_cor <- cor(A, B, method = method)
  
  # Calculate the PCC NUM_PERM times
  pTest_cors <- NUM_PERM %>% numeric
  for(i in 1:NUM_PERM) {
    permuted_B <- B %>% sample
    pTest_cors[i] <- cor(A, permuted_B, method = method)
  }
  
  # Return the rate of which we had larger correlation coefficient than the 
  # observed correlation coefficient on the original data
  return(mean(abs(pTest_cors) >= abs(obs_cor)))
}

# Generate two correlated data sets from a normal distribution based on the 
# wanted correlation level
generateData1 <- function(correlation) {
  # First generate the covariance matrix, then generate the correlated data
  cov_matrix <- matrix(c(1, correlation, correlation, 1), nrow = 2)
  return(mvrnorm(N, mu = c(0, 0), Sigma = cov_matrix))
}

# Generate two correlated data sets, one from a normal distribution, and one
# from a gamma distribution based on the wanted correlation level
generateData2 <- function(correlation) {
  # Generate data from normal distribution and from gamma distribution
  data_normal <- rnorm(N, mean = 0, sd = 1)
  data_gamma <- rgamma(N, shape = 2, scale = 2)
  data_gamma <- correlation * data_normal + sqrt(1 - correlation^2) * data_gamma
  return(data.frame(data_normal, data_gamma))
}

# Generate two correlated data sets, one from a poisson distribution, and one
# from a gamma distribution based on the wanted correlation level
generateData3 <- function(correlation) {
  # Generate data from normal distribution and from gamma distribution
  data_pois  <- rpois(N, lambda = 2)
  data_gamma <- rgamma(N, shape = 2, scale = 2)
  data_gamma <- correlation * data_pois + sqrt(1 - correlation^2) * data_gamma
  return(data.frame(data_pois, data_gamma))
}

########## TEST ##########
steps <- seq(-1, 1, by = 0.05) # Generate a vector from -1 to 1 of 0.05 steps

# Perform the test
rates_PCC  <- steps %>% length %>% numeric
rates_SRCC <- steps %>% length %>% numeric
rates_KRT  <- steps %>% length %>% numeric
for(i in 1:length(steps)) {
  PCC  <- K %>% numeric
  SRCC <- K %>% numeric
  KRT  <- K %>% numeric
  print(paste("Running correlation level:", steps[i]))
  for(k in 1:K) {
    data <- generateData3(steps[i])
    PCC[k]  <- permutationTest(data[, 1], data[, 2], method = "pearson")
    SRCC[k] <- permutationTest(data[, 1], data[, 2], method = "spearman")
    KRT[k]  <- permutationTest(data[, 1], data[, 2], method = "kendall")
  }
  rates_PCC[i]  <- mean(PCC  <= 0.05)
  rates_SRCC[i] <- mean(SRCC <= 0.05)
  rates_KRT[i]  <- mean(KRT  <= 0.05)
}

# Visualize the results
data <- data.frame(steps, rates_PCC, rates_SRCC, rates_KRT)
ggplot(data, aes(x = steps)) +
  geom_line(aes(y = rates_PCC, color = "PCC"),   linetype = "solid", 
            size = 0.5) +
  geom_line(aes(y = rates_SRCC, color = "SRCC"), linetype = "solid", 
            size = 0.5) +
  geom_line(aes(y = rates_KRT, color = "KRT"),   linetype = "solid", 
            size = 0.5) +
  geom_point(aes(y = rates_PCC, color = "PCC"),   size = 2) +
  geom_point(aes(y = rates_SRCC, color = "SRCC"), size = 2) +
  geom_point(aes(y = rates_KRT, color = "KRT"),   size = 2) +
  theme_minimal() +
  ylim(0, 1) +
  labs(title = "Test of: permutationTest",
       x = "Correlation level",
       y = TeX(r'(Rate of rejected $H_{0}$-hypotheses)')) +
  scale_color_manual(name = "Correlation coefficients:",
                     values = c("PCC" = "black", "SRCC" = "red", 
                                "KRT" = "blue")) +
  theme(legend.position = "bottom")
