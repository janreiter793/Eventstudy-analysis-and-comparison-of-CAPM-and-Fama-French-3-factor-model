################################################################################
#                                                                              #
# This code downloads historical closing prices for 30 stocks. Here 10 stocks  #
# are chosen from a small cap stocks list, 10 from a mid cap stocks list, and  #
# 10 from a large cap list. We find the adjusted R squared for the FF-3 and    #
# FF-5 trained on the data in the estimation windows for each of the five      #
# events.                                                                      #
#                                                                              #
# OVERVIEW - The script is organized into 4 sections:                          #
#   - PROGRAM PARAMETERS                                                       #
#   - LOAD FF3 FACTORS INTO R                                                  #
#   - FUNCTIONS                                                                #
#   - ANALYSIS EXECUTION                                                       #
#                                                                              #
# The ANALYSIS EXECUTION is the "main" part of of the script.                  #
#                                                                              #
# BEFORE RUNNING Program: Make sure to edit path on line 44 such that          #
#                         program can find the Fama-French factors             #
#                                                                              #
################################################################################
rm(list = ls())
set.seed(100) # Sets seed. Should be 100 to produce same results as
# given in the project
library(yahoofinancer)
library(magrittr)
library(sandwich)

########## PROGRAM PARAMETERS ##########
# Period to retrieve data from
PERIODS <- list(c('2018-02-14', '2018-08-06'),
                c('2018-03-19', '2018-09-06'),
                c('2019-04-06', '2019-11-20'),
                c('2021-10-22', '2022-04-13'),
                c('2023-03-16', '2023-09-06'))

# The names of companies used in analysis separated into large 
# cap, mid cap, and small cap
names_LCAP <- c('AAPL', 'MSFT', 'GOOG', 'GOOGL', 'AMZN',
                'NVDA', 'BRK-A', 'META', 'TSLA', 'BRK-B')

names_MCAP <- c('AMBA', 'SFIX', 'GPS',
                'HBI', 'PII', 'SAM', 'WSM', 'KEY')

names_SCAP <- c('RES', 'FCPT', 'SIX', 'MWA', 'FULT',
                'ARCO', 'HURN', 'SBLK', 'PRFZ')

# Path to the FF-3 factors
PATH <- "C:\\Users\\janre\\Documents\\uni\\7. Semester\\Projekt\\Kode\\factors_FF5.csv"

########## LOAD FF3 FACTORS INTO R ##########
loadFactorsForPeriod <- function(period) {
  # Read the factors Rm - Rf, SMB, HML, Rf
  csv <- read.csv(PATH)
  
  # Remove last row, and change date format
  csv$Date %<>% as.character %>% as.Date(format = "%Y%m%d")
  
  # Only take the subset of the factors that fall in the given period
  csv %<>%  {.[.$Date >= as.Date(period[1]) & .$Date <= as.Date(period[2]), ]}
  
  # Create a new data frame that contains all dates in the given 
  # period
  factors <- period %>% as.Date %>% {seq(.[1], .[2], "days")} %>% data.frame(Date = .)
  factors %>% merge(csv, all.x = TRUE) %>% return
}

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
    data$close %<>% {c(NA, priceToSimpleReturn(.))}
    
    # Merge stockprices and data. Data will have fewer rows than stock prices,
    # because of missing dates. Values for these missing dates will be filled
    # with NA's
    stockprices %<>% merge(data, all.x = TRUE)
    
    # Change name of column to the given stock
    colnames(stockprices)[ncol(stockprices)] <- name
  }
  
  return(stockprices)
}

# Takes a vector of prices and turn it into simple returns
priceToSimpleReturn <- function(prices) {
  prices %>% {diff(.) / .[1:(length(.) - 1)]} %>% return
}

# Takes a data frame of stock returns (with a date column) and turns
# the returns into excess returns with respect to RF
returnsToExcessReturn <- function(returns, factors) {
  for(col in names(returns[, -1])) {
    returns[[col]] %<>% {. - factors$RF}
  }
  return(returns)
}

# Takes a data frame of excess returns. Trains an FF-3 on each set of returns.
# Obtains adjusted R^2 and returns average
FF3_validation <- function(stocks, factors) {
  # Variables to store metrics
  adjRsq <- stocks %>% length %>% numeric
  
  # Go through each stock
  k <- 1
  for(stock in names(stocks[, -1])) {
    # Take all non-na entries for stock, and find corresponding
    # values for excess market return. Construct new data frame
    # with only these two vectors.
    values_stock <- stocks %>% {.[[stock]][!is.na(.[[stock]])]}
    values_factors <- stocks %>% {factors[!is.na(.[[stock]]), 2:4]}
    data <- data.frame(y = values_stock) %>% cbind(values_factors)
    rm(values_stock, values_factors)
    
    # Train an FF-5 model
    model <- lm(y ~ Mkt.RF + SMB + HML, data = data)
    adjRsq[k] <- model %>% summary %$% adj.r.squared
    k <- k + 1
  }
  
  # Calculate the mean of the metrics and return
  adjRsq %>% mean %>% return
}

# Takes a data frame of excess returns. Trains an FF-5 on each set of returns.
# Obtains adjusted R^2 and returns average
FF5_validation <- function(stocks, factors) {
  # Variables to store metrics
  adjRsq <- stocks %>% length %>% numeric
  
  # Go through each stock
  k <- 1
  for(stock in names(stocks[, -1])) {
    # Take all non-na entries for stock, and find corresponding
    # values for excess market return. Construct new data frame
    # with only these two vectors.
    values_stock <- stocks %>% {.[[stock]][!is.na(.[[stock]])]}
    values_factors <- stocks %>% {factors[!is.na(.[[stock]]), 2:6]}
    data <- data.frame(y = values_stock) %>% cbind(values_factors)
    rm(values_stock, values_factors)

    # Train an FF-5 model
    model <- lm(y ~ Mkt.RF + SMB + HML + RMW + CMA, data = data)
    adjRsq[k] <- model %>% summary %$% adj.r.squared
    k <- k + 1
  }
  
  # Calculate the mean of the metrics and return
  adjRsq %>% mean %>% return
}

########## ANALYSIS EXECUTION ##########
# Check if all companies are listed on yahoo finance
c(names_LCAP, names_MCAP, names_SCAP) %>% isListed

values <- numeric(6) %>% data.frame()

for(period in PERIODS) {
  paste("Running period:", period[1], period[2]) %>% print
  
  # Retrieve the prices for all the stocks
  print("Retreiving prices.")
  smallcap_return <- retrievePrices(period, names_SCAP)
  midcap_return   <- retrievePrices(period, names_MCAP)
  largecap_return <- retrievePrices(period, names_LCAP)
  
  # Load factors relevant for period
  print("Loading factors relevant for period.")
  factors <- period %>% loadFactorsForPeriod
  
  # Turn stock returns into excess returns
  print("Calculating excess returns.")
  smallcap_return %<>% returnsToExcessReturn(factors)
  midcap_return   %<>% returnsToExcessReturn(factors)
  largecap_return %<>% returnsToExcessReturn(factors)
  
  print("Finding adjusted R^2.")
  # Run validation using FF3
  FF3_s <- smallcap_return %>% FF3_validation(factors)
  FF3_m <- midcap_return   %>% FF3_validation(factors)
  FF3_l <- largecap_return %>% FF3_validation(factors)
  
  # Run validation using FF5
  FF5_s <- smallcap_return %>% FF5_validation(factors)
  FF5_m <- midcap_return   %>% FF5_validation(factors)
  FF5_l <- largecap_return %>% FF5_validation(factors)
  
  values <- c(FF3_s, FF3_m, FF3_l, FF5_s, FF5_m, FF5_l) %>% {cbind(values, .)}
}

values %<>% {.[-1]}
colnames(values) <- c("Buy back TSLA", "Marijuana", "Cybertruck", 
                      "Twitter Offer", "Starlink")
rownames(values) <- c("FF3_s", "FF3_m", "FF3_l", "FF5_s", "FF5_m", "FF5_l")
values %>% print
