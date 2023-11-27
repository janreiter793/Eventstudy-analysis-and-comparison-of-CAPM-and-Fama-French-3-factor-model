################################################################################
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
library(boot)

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

# Parameters
validation_split <- 0.2 # The percentage of data assigned to a test data set.

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

# Takes a vector of prices and turn it into simple returns
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

# Takes a data frame of excess returns. Trains a naive on each set of returns.
# Obtains MAE, RMSE, and adjusted R^2 and returns average of the metrics
NAIVE_validation <- function(stocks) {
  # Variables to store metrics
  k <- stocks %>% length
  MAE    <- numeric(k)
  RMSE   <- numeric(k)
  adjRsq <- numeric(k)
  
  # Go through each stock
  k <- 1
  for(stock in names(stocks[, -1])) {
    data <- stocks[[stock]] %>% na.omit %>% {data.frame(y = .)}
    
    # split data into a training set and a test set
    n <- data %>% nrow
    split <- round(n * (1 - validation_split))
    training_data <- data[1:split, ]       %>% data.frame(y = .)
    test_data     <- data[(split + 1):n, ] %>% data.frame(y = .)
    
    # Train a naive model
    last_val <- training_data$y %>% tail(1)
    training_data$centered <- training_data$y - last_val
    model <- lm(centered ~ 1, data = training_data)
    
    # Make predictions on the test set, transform excess returns into
    # simple returns again
    predictions <- predict(model, newdata = test_data)
    predictions %<>% unname
    
    # Obtain the values of metrics
    MAE[k]    <- (predictions - test_data$y) %>% abs  %>% mean
    RMSE[k]   <- ((predictions - test_data$y)^2) %>% mean %>% sqrt
    adjRsq[k] <- model %>% summary %$% adj.r.squared
    k <- k + 1
  }
  
  # Calculate the mean of the metrics and return
  mean_metric <- list(MAE = mean(MAE), RMSE = mean(RMSE), adjRsq = mean(adjRsq))
  return(mean_metric)
}

# Takes a data frame of excess returns. Trains a CAPM on each set of returns.
# Obtains MAE, RMSE, and adjusted R^2 and returns average of the metrics
CAPM_validation <- function(stocks) {
  # Variables to store metrics
  k <- stocks %>% length
  MAE    <- numeric(k)
  RMSE   <- numeric(k)
  adjRsq <- numeric(k)
  
  # Go through each stock
  k <- 1
  for(stock in names(stocks[, -1])) {
    # Take all non-na entries for stock, and find corresponding
    # values for excess market return. Construct new data frame
    # with only these two vectors.
    values_stock <- stocks %>% {.[[stock]][!is.na(.[[stock]])]}
    values_factors <- stocks %>% {factors[!is.na(.[[stock]]), 2]}
    data <- data.frame(y = values_stock, x = values_factors)
    rm(values_stock, values_factors)
    
    # split data into a training set and a test set
    n <- data %>% nrow
    split <- round(n * (1 - validation_split))
    training_data <- data[1:split, ]
    test_data     <- data[(split + 1):n, ]
    
    # Train a CAPM
    model <- lm(y ~ x, data = training_data)
    
    # Make predictions on the test set, transform excess returns into
    # simple returns again
    predictions <- predict(model, newdata = test_data)
    predictions %<>% unname
    
    # Obtain the values of metrics
    MAE[k]    <- (predictions - test_data$y) %>% abs  %>% mean
    RMSE[k]   <- ((predictions - test_data$y)^2) %>% mean %>% sqrt
    adjRsq[k] <- model %>% summary %$% adj.r.squared
    k <- k + 1
  }
  
  # Calculate the mean of the metrics and return
  mean_metric <- list(MAE = mean(MAE), RMSE = mean(RMSE), adjRsq = mean(adjRsq))
  return(mean_metric)
}

# Takes a data frame of excess returns. Trains an FF3 on each set of returns.
# Obtains MAE, RMSE, and adjusted R^2 and returns average of metrics
FF3_validation <- function(stocks) {
  # Variables to store metrics
  k <- stocks %>% length
  MAE    <- numeric(k)
  RMSE   <- numeric(k)
  adjRsq <- numeric(k)
  
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
    
    # split data into a training set and a test set
    n <- data %>% nrow
    split <- round(n * (1 - validation_split))
    training_data <- data[1:split, ]
    test_data     <- data[(split + 1):n, ]
    
    # Train an FF-3 model
    model <- lm(y ~ Mkt.RF + SMB + HML, data = training_data)
    
    # Make predictions on the test set, transform excess returns into
    # simple returns again
    predictions <- predict(model, newdata = test_data)
    predictions %<>% unname
    
    # Obtain the values of metrics
    MAE[k]    <- (predictions - test_data$y) %>% abs  %>% mean
    RMSE[k]   <- ((predictions - test_data$y)^2) %>% mean %>% sqrt
    adjRsq[k] <- model %>% summary %$% adj.r.squared
    k <- k + 1
  }
  
  # Calculate the mean of the metrics and return
  mean_metric <- list(MAE = mean(MAE), RMSE = mean(RMSE), adjRsq = mean(adjRsq))
  return(mean_metric)
}

########## ANALYSIS EXECUTION ##########
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

# Run validation using NAIVE model
naive_s <- smallcap_return %>% NAIVE_validation
naive_m <- midcap_return   %>% NAIVE_validation
naive_l <- largecap_return %>% NAIVE_validation

# Run validation using CAPM
CAPM_s <- smallcap_return %>% CAPM_validation
CAPM_m <- midcap_return   %>% CAPM_validation
CAPM_l <- largecap_return %>% CAPM_validation

# Run validation using FF3
FF3_s <- smallcap_return %>% FF3_validation
FF3_m <- midcap_return   %>% FF3_validation
FF3_l <- largecap_return %>% FF3_validation

# Neat data-frame to summarize the results
conclusion <- data.frame(MAE.s = c(naive_s$MAE, CAPM_s$MAE, FF3_s$MAE),
                         MAE.m = c(naive_m$MAE, CAPM_m$MAE, FF3_m$MAE),
                         MAE.l = c(naive_l$MAE, CAPM_l$MAE, FF3_l$MAE),
                         RMSE.s = c(naive_s$RMSE, CAPM_s$RMSE, FF3_s$RMSE),
                         RMSE.m = c(naive_m$RMSE, CAPM_m$RMSE, FF3_m$RMSE),
                         RMSE.l = c(naive_l$RMSE, CAPM_l$RMSE, FF3_l$RMSE),
                         adjRsq.s = c(naive_s$adjRsq, CAPM_s$adjRsq, FF3_s$adjRsq),
                         adjRsq.m = c(naive_m$adjRsq, CAPM_m$adjRsq, FF3_m$adjRsq),
                         adjRsq.l = c(naive_l$adjRsq, CAPM_l$adjRsq, FF3_l$adjRsq))
row.names(conclusion) <- c('Naive Model', 'CAPM', 'FF3')
conclusion %>% print