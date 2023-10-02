#
# OVERVIEW - The script is organized into 4 sections:
#   - PROGRAM PARAMETERS
#   - LOAD FF3 FACTORS INTO R
#   - FUNCTIONS
#   - ANALYSIS EXECUTION
#
# The ANALYSIS EXECUTION is the "main" part of of the script.
#
# BEFORE RUNNING Program: Make sure to edit path on line 35 such that
#                         program can find the Fama-French factors
#
rm(list = ls())
library(yahoofinancer)
library(magrittr)
library(zoo)
library(caret)

########## PROGRAM PARAMETERS ##########
# Period to retrieve data from
PERIOD <- c('2003-01-01', '2022-01-01')

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

# Parameter for K-fold cross-validation
K <- 10

########## LOAD FF3 FACTORS INTO R ##########
# Read the factors Rm - Rf, SMB, HML, Rf
csv <- read.csv(PATH)

# Remove last row, and change date format
csv %<>% {.[-nrow(.), ]}
csv$Date %<>% as.Date(format = "%Y%m%d")

# Only take the subset of the factors that fall in the given period
csv %<>%  {.[.$Date >= as.Date(PERIOD[1]) & .$Date <= as.Date(PERIOD[2]), ]}

# Create a new data frame that contains all dates in the given 
# period and fill in NA's with linear interpolation, and 
# last/first known value for NA's at the end/beginning of the
# dataframe
factors <- PERIOD %>% as.Date %>% {seq(.[1], .[2], "days")} %>% data.frame(Date = .)
factors %<>% merge(csv, all.x = TRUE)
rm(csv)
for(factor in names(factors[, -c(1)])) {
  factors[[factor]] %<>% na.approx(na.rm = FALSE)
  factors[[factor]] %<>% na.locf(na.rm = FALSE)
  factors[[factor]] %<>% rev %>% na.locf(na.rm = FALSE) %>% rev
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
# closing prices for the stocks over a given period in a data frame and
# uses linear interpolation to fill in NA's.
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
    
    # Use linear interpolation
    stockprices$close %<>% na.approx(na.rm = FALSE)
    stockprices$close %<>% na.locf(na.rm = FALSE) 
    
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

# Function for K-fold cross-validation, can do K-fold for naïve models,
# CAPM and FF3.
k_fold_cross_validation <- function(data, type) {
  # Create all the folds
  folds <- createFolds(data$excess_return, k = K, list = TRUE)
  
  # Vectors to store performance metrics
  MAE    <- numeric(K) # Mean absolute error
  RMSE   <- numeric(K) # Root mean square error
  Rsq    <- numeric(K) # R-squared
  adjRsq <- numeric(K) # Adjusted R-squared
  aic    <- numeric(K) # Akaikes Information Criterion
  
  for (i in 1:K) {
    # Create training and testing sets for the current fold
    train_data <- folds[-i]  %>% unname %>% unlist %>% {data[., ]}
    test_data  <- folds[[i]] %>% unname %>% unlist %>% {data[., ]}
    
    # Train the model
    if(type == "naive") {
      # Train the Naïve model, by using LM on the data centered around
      # the last value.
      last_val <- train_data$excess_return %>% tail(1)
      train_data$centered <- train_data$excess_return - last_val
      model <- lm(centered ~ 1, data = train_data)
    } else if(type == "capm") {
      # Fit CAPM by using OLS
      model <- lm(excess_return ~ Mkt.RF, data = train_data)
    } else if(type == "ff3"){
      # Fit FF3 by OLS
      model <- lm(excess_return ~ Mkt.RF + SMB + HML, data = train_data)
    } else {
      print("No proper type was given.")
      return()
    }
    
    # Make predictions on the test set, transform excess returns into
    # simple returns again
    predictions <- predict(model, newdata = test_data)
    predictions %<>% unname

    # Evaluate model performance 
    MAE[i]    <- (predictions - test_data$excess_return)     %>% abs  %>% mean
    RMSE[i]   <- ((predictions - test_data$excess_return)^2) %>% mean %>% sqrt
    Rsq[i]    <- model %>% summary %$% r.squared
    adjRsq[i] <- model %>% summary %$% adj.r.squared
    aic[i]    <- model %>% AIC
  }
  
  # Return the average metrics across all folds
  mean_metric <- list(MAE = mean(MAE), RMSE = mean(RMSE),
                      Rsq = mean(Rsq), adjRsq = mean(adjRsq), 
                      aic = mean(aic))
  return(mean_metric)
}

# Performs K-fold cross-validation on the CAPM, FF3 and NAÏVE model 
# trained on a stocks from a given data frame
NAIVE_CAPM_FF3_cross_validate <- function(stocks) {
  NAIVE_Results <- list()
  CAPM_Results  <- list()
  FF3_Results   <- list()
  
  for(stock in names(stocks[, -c(1)])) {
    # Create a data frame containing the stocks excess returns and
    # all the FF3 factors
    values_stock <- stocks %>% {.[[stock]][!is.na(.[[stock]])]}
    values_factors <- stocks %>% {factors[!is.na(.[[stock]]), -1]}
    data <- data.frame(excess_return = values_stock)
    data %<>% cbind(values_factors) 
    rm(values_stock, values_factors)
    
    # Performs the K-fold cross validation
    cv_NAIVE <- k_fold_cross_validation(data, type = "naive")
    cv_CAPM  <- k_fold_cross_validation(data, type = "capm")
    cv_FF3   <- k_fold_cross_validation(data, type = "ff3")
    
    # Store the results
    NAIVE_Results[[stock]] <- cv_NAIVE
    CAPM_Results[[stock]]  <- cv_CAPM
    FF3_Results[[stock]]   <- cv_FF3
  }
  
  return(list(NAIVE_Results, CAPM_Results, FF3_Results))
}

# Calculate the average of the performance metrics across models
# for each stock
calc_average_metrics <- function(metrics) {
  temp <- metrics %>% {do.call(rbind, .)}
  temp %<>%  {matrix(as.numeric(.), nrow = nrow(.), ncol = ncol(.))}
  temp %>% colMeans %>% return
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

# Perform the K-fold cross-validation on Naive, CAPM and FF3 with
# all the stocks
res <- smallcap_return %>% NAIVE_CAPM_FF3_cross_validate
NAIVE_SCAP_Results <- res[[1]]
CAPM_SCAP_Results  <- res[[2]]
FF3_SCAP_Results   <- res[[3]]

res <- midcap_return %>% NAIVE_CAPM_FF3_cross_validate
NAIVE_MCAP_Results <- res[[1]]
CAPM_MCAP_Results  <- res[[2]]
FF3_MCAP_Results   <- res[[3]]

res <- largecap_return %>% NAIVE_CAPM_FF3_cross_validate
NAIVE_LCAP_Results <- res[[1]]
CAPM_LCAP_Results  <- res[[2]]
FF3_LCAP_Results   <- res[[3]]

# Free some memory
rm(smallcap_return, midcap_return, largecap_return, factors, res)

# Calculate the mean of all the metrics
naive_scap_average_metrics <- NAIVE_SCAP_Results %>% calc_average_metrics
naive_mcap_average_metrics <- NAIVE_MCAP_Results %>% calc_average_metrics
naive_lcap_average_metrics <- NAIVE_LCAP_Results %>% calc_average_metrics
capm_scap_average_metrics  <- CAPM_SCAP_Results  %>% calc_average_metrics
capm_mcap_average_metrics  <- CAPM_MCAP_Results  %>% calc_average_metrics
capm_lcap_average_metrics  <- CAPM_LCAP_Results  %>% calc_average_metrics
ff3_scap_average_metrics   <- FF3_SCAP_Results   %>% calc_average_metrics
ff3_mcap_average_metrics   <- FF3_MCAP_Results   %>% calc_average_metrics
ff3_lcap_average_metrics   <- FF3_LCAP_Results   %>% calc_average_metrics

# Neat data-frame to summarize the results
conclusion <- data.frame(MAE.s = c(naive_scap_average_metrics[1], capm_scap_average_metrics[1], ff3_scap_average_metrics[1]),
                         MAE.m = c(naive_mcap_average_metrics[1], capm_mcap_average_metrics[1], ff3_mcap_average_metrics[1]),
                         MAE.l = c(naive_lcap_average_metrics[1], capm_mcap_average_metrics[1], ff3_lcap_average_metrics[1]),
                         RMSE.s = c(naive_scap_average_metrics[2], capm_scap_average_metrics[2], ff3_scap_average_metrics[2]),
                         RMSE.m = c(naive_mcap_average_metrics[2], capm_mcap_average_metrics[2], ff3_mcap_average_metrics[2]),
                         RMSE.l = c(naive_lcap_average_metrics[2], capm_mcap_average_metrics[2], ff3_lcap_average_metrics[2]),
                         Rsq.s = c(naive_scap_average_metrics[3], capm_scap_average_metrics[3], ff3_scap_average_metrics[3]),
                         Rsq.m = c(naive_mcap_average_metrics[3], capm_mcap_average_metrics[3], ff3_mcap_average_metrics[3]),
                         Rsq.l = c(naive_lcap_average_metrics[3], capm_mcap_average_metrics[3], ff3_lcap_average_metrics[3]),
                         adjRsq.s = c(naive_scap_average_metrics[4], capm_scap_average_metrics[4], ff3_scap_average_metrics[4]),
                         adjRsq.m = c(naive_mcap_average_metrics[4], capm_mcap_average_metrics[4], ff3_mcap_average_metrics[4]),
                         adjRsq.l = c(naive_lcap_average_metrics[4], capm_mcap_average_metrics[4], ff3_lcap_average_metrics[4]),
                         AIC.s = c(naive_scap_average_metrics[5], capm_scap_average_metrics[5], ff3_scap_average_metrics[5]),
                         AIC.m = c(naive_mcap_average_metrics[5], capm_mcap_average_metrics[5], ff3_mcap_average_metrics[5]),
                         AIC.l = c(naive_lcap_average_metrics[5], capm_mcap_average_metrics[5], ff3_lcap_average_metrics[5]))
row.names(conclusion) <- c('Naive Model', 'CAPM', 'FF3')
conclusion %>% print
