################################################################################
#                                                                              #
# This code downloads historical closing prices for 30 stocks in the period    #
# from 2002-01-01 to 2022-01-0. Here 10 stocks are chosen from a small cap     #
# stocks list, 10 from a mid cap stocks list, and 10 from a large cap list.    #
# Then we use these 30 stocks to test the CAPM.                                #
#                                                                              #
# OVERVIEW - The script is organized into 4 sections:                          #
#   - PROGRAM PARAMETERS                                                       #
#   - LOAD FF3 FACTORS INTO R                                                  #
#   - FUNCTIONS                                                                #
#   - ANALYSIS EXECUTION                                                       #
#                                                                              #
# The ANALYSIS EXECUTION is the "main" part of of the script.                  #
#                                                                              #
# BEFORE RUNNING Program: Make sure to edit path on line 35 such that          #
#                         program can find the Fama-French factors             #
#                                                                              #
################################################################################
rm(list = ls())
set.seed(100) # Sets seed. Should be 100 to produce same results as
              # given in the project
library(yahoofinancer)
library(magrittr)
library(zoo)
library(caret)
library(ggplot2)
library(boot)
library(gridExtra)

########## PROGRAM PARAMETERS ##########
# Period to retrieve data from
PERIOD <- c('2002-01-01', '2022-01-01')

# The names of companies used in analysis separated into large 
# cap, mid cap, and small cap
names_LCAP <- c('AAPL', 'MSFT', 'GOOG', 'GOOGL', 'AMZN',
                'NVDA', 'BRK-A', 'META', 'TSLA', 'BRK-B')

names_MCAP <- c('AMBA', 'CLOV', 'SFIX', 'ETRN', 'GPS',
                'HBI', 'PII', 'SAM', 'WSM', 'KEY')

names_SCAP <- c('RES', 'FCPT', 'SIX', 'MWA', 'FULT',
                'ARCO', 'HURN', 'SBLK', 'BMBL', 'PRFZ')

# Path to the FF-3 factors
PATH <- "C:\\Users\\janre\\Documents\\uni\\7. Semester\\Projekt\\Kode\\factors.csv"

# Parameters:
R        <- 1000 # number of bootstrap samples
NUM_PERM <- 1000 # Number of permutations used in permutation test

########## LOAD FF3 FACTORS INTO R ##########
# Read the factors Rm - Rf, SMB, HML, Rf
csv <- read.csv(PATH)

# Remove last row, and change date format
csv %<>% {.[-nrow(.), ]}
csv$Date %<>% as.Date(format = "%Y%m%d")

# Only take the subset of the factors that fall in the given period
csv %<>%  {.[.$Date >= as.Date(PERIOD[1]) & .$Date <= as.Date(PERIOD[2]), ]}

# Create a new data frame that contains all dates in the given 
# period
factors <- PERIOD %>% as.Date %>% {seq(.[1], .[2], "days")} %>% data.frame(Date = .)
factors %<>% merge(csv, all.x = TRUE)
rm(csv)

########## FUNCTIONS ##########
# This function checks if stock-names are listed on yahoo finance
isListed <- function(names) {
  success <- TRUE
  for(name in names)
    if(!(name %>% validate)) { success <- FALSE }
  return(success)
}

# This function takes a period and a list of stock names, stores all daily
# closing prices for the stocks over a given period in a data frame
retrievePrices <- function(period, names) {
  # The data frame that stores all closing prices, contains all dates in the
  # period.
  stockprices <- period %>% as.Date %>% {seq(.[1], .[2], "days")} %>% data.frame(date = .)
  
  # Go through each name in list
  for(name in names) {
    # Retrieve the stock data
    stock <- name %>% Ticker$new(.)
    data <- stock$get_history(start = period[1], end = period[2], interval = "1d")
    
    # Remove time stamp from data, and remove all columns except 'date',
    # and 'close'
    data$date %<>% as.Date
    data %<>% {.[, c('date', 'close')]}
    
    # Merge stockprices and data. Data will have fewer rows than stock prices,
    # because of missing dates. Values for these missing dates will be filled
    # with NA's
    stockprices %<>% merge(data, all.x = TRUE)
    
    # Change name of column to the given stock
    colnames(stockprices)[ncol(stockprices)] <- name
  }
  
  return(stockprices)
}

# Takes a vector of prices and turns it into simple returns
priceToSimpleReturn <- function(prices) {
  prices %>% {diff(.) / .[1:(length(.) - 1)]} %>% return
}

# Takes a data frame with the dates and stock prices, and returns
# the a data frame with simple returns instead of prices
df_priceToSimpleReturn <- function(data) {
  # Ignore the first column as it only contains dates
  for(col in names(data[, -c(1)])) {
    # Take column of prices, calculate simple return and add NA at the 
    # beginning, such that return at time t corresponds to the return 
    # earned from time t-1 to t.
    data[[col]] %<>% {c(NA, priceToSimpleReturn(.))}
  }
  return(data)
}

# Takes a data frame of stock returns (with a date column) and turns
# the returns into excess returns with respect to RF
returnsToExcessReturn <- function(returns) {
  for(col in names(returns[, -1])) {
    returns[[col]] %<>% {. - factors$RF}
  }
  return(returns)
}

# Returns the intercept of an LM trained on df
extract_intercept <- function(df, indices) {
  model <- lm(y ~ x, data = df[indices, ])
  return(coef(model)[[1]])
}

# Run bootstrap on models trained for each of the 30 stocks, return
# the percentage of the models which has intercept zero
interceptTest <- function(stocks) {
  res <- 0
  
  # Go through each column except for the first which contains
  # only dates
  for(stock in names(stocks[, -1])) {
    # Take all non-na entries for stock, and find corresponding
    # values for excess market return. Construct new dataframe
    # with only these two vectors.
    values_stock <- stocks %>% {.[[stock]][!is.na(.[[stock]])]}
    values_factors <- stocks %>% {factors[!is.na(.[[stock]]), 2]}
    data <- data.frame(y = values_stock, x = values_factors)
    rm(values_stock, values_factors)
    
    # Perform Bootstrap to obtain confidence interval for the
    # intercept in the CAPM
    print(paste("Running Bootstrap on:", stock))
    boot_results <- boot(data = data, statistic = extract_intercept, R = R)
    boot_ci <- boot.ci(boot_results, type = "basic")
    print(boot_ci$basic[c(4, 5)])
    
    # If confidence interval does not contain 0, then we reject
    # the null hypothesis (alpha = 0), hence we add one to res
    if(!(boot_ci$basic[4] <= 0 & 0 <= boot_ci$basic[5])) { res <- res + 1 }
  }
  return(res / length(stocks[, -1]))
}

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

# Runs permutation test between each set of stock returns against factors$Mkt.RF
# using Pearson's correlation coefficient, and returns a list of p-values.
correlationTest_Pearson <- function(stocks) {
  results <- list()
  for(name in names(stocks[, -1])) {
    # Aligning returns with factors, so we have no NA-values, and factors are
    # aligned with the correct stock return values
    values_stock <- stocks %>% {.[[name]][!is.na(.[[name]])]}
    values_factors <- stocks %>% {factors[!is.na(.[[name]]), 2]}
    data <- data.frame(y = values_stock, x = values_factors)
    rm(values_stock, values_factors)
    
    # Obtain the p-values from the permutation test using PCC.
    model <- lm(y ~ x, data = data)
    residuals <- model %>% resid
    print(paste("Correlationtesting:", name))
    results[[name]] <- permutationTest(residuals, data$x, method = "pearson")
  }
  results %>% return
}

# Takes a dataframe of stock returns. Plots CAPM residuals for each vector of
# returns against factors$Mkt.RF shape in a neat 2x5-plots-figure, to visualize
# the correlation. (May take some time to run...)
correlationsPlotFactors <- function(stocks) {
  plots <- list()
  for(name in names(stocks[, -1])) {
    # Aligning returns with factors, so we have no NA-values, and factors are
    # aligned with the correct stock return values
    values_stock <- stocks %>% {.[[name]][!is.na(.[[name]])]}
    values_factors <- stocks %>% {factors[!is.na(.[[name]]), 2]}
    data <- data.frame(y = values_stock, x = values_factors)
    rm(values_stock, values_factors)
    
    # Fit the model, obtain the residuals, and generate the correlation plot.
    model <- lm(y ~ x, data = data)
    data <- model %>% resid %>% {data.frame(y = ., x = data$x)}
    plots[[name]] <- ggplot(data, aes(x = x, y = y)) +
      geom_point() +
      theme_minimal() +
      labs(title = name, 
           subtitle = paste("PCC:", cor(data$y, data$x)),
           x = "Excess Market Return",
           y = "Residuals")
  }
  
  # Return the 2x5-plots-figure
  plots %>% {c(., ncol = 5)} %>% {do.call(grid.arrange, .)} %>% return
}

########## ANALYSIS EXECUTION ##########
## Setup - We retrieve the stockprices, transform them into simple
#          Excess returns

# Check if all companies are listed on yahoo finance
c(names_LCAP, names_MCAP, names_SCAP) %>% isListed

# Retrieve the prices for all the stocks
smallcap_prices <- retrievePrices(PERIOD, names_SCAP)
midcap_prices   <- retrievePrices(PERIOD, names_MCAP)
largecap_prices <- retrievePrices(PERIOD, names_LCAP)

# Transform the prices into returns
smallcap_return <- smallcap_prices %>% df_priceToSimpleReturn
midcap_return   <- midcap_prices   %>% df_priceToSimpleReturn
largecap_return <- largecap_prices %>% df_priceToSimpleReturn

# Remove unnecessary data frames
rm(smallcap_prices, midcap_prices, largecap_prices)

# Turn stock returns into excess returns
smallcap_return %<>% returnsToExcessReturn 
midcap_return   %<>% returnsToExcessReturn
largecap_return %<>% returnsToExcessReturn

## Test for normality...
#
# ...
#

## Test for exogeneity - To make sure we have unbiased, and consistent 
# estimator, we check for exogeneity by using correlation coefficient tests.

# Find the number of values that occur more than once in factors$Mkt.RF, find
# the overall number of different values, then calculate the percentage of 
# values that occur more than once
ties        <- factors$Mkt.RF %>% table %>% {. > 1} %>% sum
unique_nums <- factors$Mkt.RF %>% table %>% length
print(paste("Percentage of values that occour more than once:", ties/unique_nums))
rm(ties, unique_nums)

# Because of many ties, we only use the permutation test with the Pearson
# correlation coefficient
scap_pval <- smallcap_return %>% correlationTest_Pearson
mcap_pval <- midcap_return   %>% correlationTest_Pearson
lcap_pval <- largecap_return %>% correlationTest_Pearson

# Calculate the rate of accepted H_0-hypotheses. (A rejection implies 
# that residuals are correlated with the factors)
print("Rate of CAPMs where residuals are uncorrelated with excess market return:")
scap_pval %>% unlist %>% sum %>% {. / 10} %>% {paste("Small cap:", .)} %>% print
mcap_pval %>% unlist %>% sum %>% {. / 10} %>% {paste("Mid cap:  ", .)} %>% print
lcap_pval %>% unlist %>% sum %>% {. / 10} %>% {paste("Large cap:", .)} %>% print

# We supplement the tests visually by plotting the residuals against the excess
# market returns.
smallcap_return %>% correlationsPlotFactors
midcap_return %>% correlationsPlotFactors
largecap_return %>% correlationsPlotFactors

## Test for intercept
# Check the significance of the intercept
intercept_s <- smallcap_return %>% interceptTest
intercept_m <- midcap_return   %>% interceptTest
intercept_l <- largecap_return %>% interceptTest

# Results
print("The rates at which the intercept was significant:")
print(paste("Smallcap:", intercept_s))
print(paste("Midcap:  ", intercept_m))
print(paste("Largecap:", intercept_l))
