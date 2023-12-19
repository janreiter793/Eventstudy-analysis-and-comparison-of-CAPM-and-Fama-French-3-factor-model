################################################################################
#                                                                              #
# This code downloads historical closing prices for 30 stocks in the period    #
# from 2002-01-01 to 2022-01-0. Here 10 stocks are chosen from a small cap     #
# stocks list, 10 from a mid cap stocks list, and 10 from a large cap list.    #
# This data is used to obtain performance metrics of the naïve, the CAPM, and  #
# the FF-3 model to asses which one is the best fit. Furhtermore SUR-models    #
# are used in conjunction with the Wald test to test if the paramters associ-  #
# ated with HML and SMB are statistically significant.                         #
#                                                                              #
# OVERVIEW - The script is organized into 4 sections:                          #
#   - PROGRAM PARAMETERS                                                       #
#   - LOAD FF3 FACTORS INTO R                                                  #
#   - FUNCTIONS                                                                #
#   - ANALYSIS EXECUTION                                                       #
#                                                                              #
# The ANALYSIS EXECUTION is the "main" part of of the script.                  #
#                                                                              #
# BEFORE RUNNING Program: Make sure to edit path on line 46 such that          #
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
PATH <- "C:\\...\\factors.csv"

# Parameters
validation_split <- 0.2  # The percentage of data assigned to a test data set.
alpha            <- 0.05 # Significance level

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

# Takes a list of vectors of dependent variables and a list of data frames of
# independent variables, then estimates a SUR-model using OLS
surfit <- function(dependent, independent, intercept = TRUE) {
  # Takes all the vectors of dependent variables and stack them onto each other
  Y <- dependent %>% unlist
  
  # If intercept is wanted, then add a column of ones to each dataframe
  if(intercept) {
    independent %<>% lapply(function(df) df %<>% nrow %>% {rep(1, .)} %>% {cbind(., df)})
  }
  
  # Find number of rows, columns and number of equations
  rows <- independent %>% lapply(function(df) nrow(df)) %>% unname %>% unlist
  cols <- independent %>% lapply(function(df) ncol(df)) %>% unname %>% unlist
  ncols <- cols %>% sum
  g <- independent %>% length
  
  # Create block matrix X by first adding the first dataframe of independent
  # variables, then adding zeros on
  X <- independent[[1]] %>% as.matrix
  X %<>% cbind(matrix(0, ncol = ncols - cols[1], nrow = rows[1]))
  
  # Then add all the other blocks of regressors and zeros
  for(i in 2:g) {
    if(g < 2) { break}
    m <- rows[i]
    n <- cols[1:(i - 1)] %>% sum
    zero_matrix <- matrix(0, ncol = n, nrow = m)
    zero_matrix %<>% cbind(as.matrix(independent[[i]]))
    if(i < g) {
      n <- cols[(i + 1):g] %>% sum
      zero_matrix_2 <- matrix(0, ncol = n, nrow = m)
      zero_matrix %<>% cbind(zero_matrix_2)
    }
    X %<>% rbind(zero_matrix)
  }
  X %<>% unname
  
  # Fit the model by using OLS
  lm(Y ~ X + 0) %>% return
}

# Takes a data frame of stock returns, and creates a new list where for each
# stock there are columns of Mkt.RF, HML, and SMB with only the values that 
# corresponds to the available stock returns.
aligndataframes <- function(stocks) {
  # Aligning returns with factors, so we have no NA-values, and factors are
  # aligned with the correct stock return values
  liste <- list()
  for(name in names(stocks[, -1])) {
    values <- data.frame(stocks[[name]], 
                         Mkt.RF = factors$Mkt.RF,
                         HML = factors$HML,
                         SMB = factors$SMB)
    values %<>% na.omit
    liste[[name]] <- data.frame(values$Mkt.RF, values$HML, values$SMB)
  }
  return(liste)
}

# Takes a dataframe of stock returns, omits NA-values and collect them in a list
listifyStocks <- function(stocks) {
  liste <- list()
  for(name in names(stocks[, -1])) {
    liste[[name]] <- na.omit(stocks[[name]])
  }
  return(liste)
}

# Performs a Wald test to test if a model may be restricted
Waldtest <- function(model, V, R) {
  theta <- model$coefficients %>% as.matrix
  n     <- model$residuals %>% length
  P     <- model$coefficients %>% length
  Q     <- R %>% nrow
  
  # Compute Wald statistic
  statistic <- t(R %*% theta) %*% solve(V) %*% (R %*% theta)
  
  # Returns degrees of freedom and the test statistic
  return(list(Q, statistic))
}

########## ANALYSIS EXECUTION ##########
# Check if all companies are listed on yahoo finance
c(names_LCAP, names_MCAP, names_SCAP) %>% isListed

# Retrieve the prices for all the stocks
smallcap_return <- retrievePrices(PERIOD, names_SCAP)
midcap_return   <- retrievePrices(PERIOD, names_MCAP)
largecap_return <- retrievePrices(PERIOD, names_LCAP)

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

## Use SUR-models and Wald tests to asses whether the coefficients for HML and
# SMB are statistically significant. Adjust for heteroscedasticity using HC3.
# Fit a SUR-model with all the FF-3 models
combined <- cbind(smallcap_return, midcap_return[-1], largecap_return[-1])
y <- listifyStocks(combined)
x <- aligndataframes(combined)
SUR_model_combined <- surfit(y, x)

# Perform Wald test to test if parameters for HML and SMB
R <- diag(1, nrow = 30, ncol = 30)
hypothesisMatrix = c(0, 0, 1, 0,
                     0, 0, 0, 1) %>% matrix(ncol = 4, nrow = 2, byrow = TRUE)
R %<>% kronecker(hypothesisMatrix)
index <- c(seq(3, by = 4, length.out = 30), seq(4, by = 4, length.out = 30))
covariance_mat_comb  <- SUR_model_combined %>% vcovHC(type = "HC3") %>% {.[index, index]}
res_comb <- SUR_model_combined %>% Waldtest(covariance_mat_comb, R)

# Find 95% confidence region
Q_combined <- res_comb[[1]]
critical_value <- qchisq(1 - alpha, df = Q_combined)
{print(paste("95% critical value for combined model:", critical_value))
  print(paste("Wald test statistic and p-value for restricted model (combined):", res_comb[2],
              " and ", 1 - pchisq(res_comb[[2]], df = Q_combined)))}

# Fit a SUR-model on each of the three size groups
y <- listifyStocks(smallcap_return)
x <- aligndataframes(smallcap_return)
SUR_model_smallcap <- surfit(y, x)

# For mid cap
y <- listifyStocks(midcap_return)
x <- aligndataframes(midcap_return)
SUR_model_midcap   <- surfit(y, x)

# For large cap
y <- listifyStocks(largecap_return)
x <- aligndataframes(largecap_return)
SUR_model_largecap <- surfit(y, x)

# Perform Wald test on each of the three SUR-models to asses whether HML and
# SMB are significant or not
R <- diag(1, nrow = 10, ncol = 10)
R %<>% kronecker(hypothesisMatrix)
index <- c(seq(3, by = 4, length.out = 10), seq(4, by = 4, length.out = 10))
covariance_mat_small <- SUR_model_smallcap %>% vcovHC(type = "HC3") %>% {.[index, index]}
covariance_mat_mid   <- SUR_model_midcap   %>% vcovHC(type = "HC3") %>% {.[index, index]}
covariance_mat_large <- SUR_model_largecap %>% vcovHC(type = "HC3") %>% {.[index, index]}
res_small <- SUR_model_smallcap %>% Waldtest(covariance_mat_small, R)
res_mid   <- SUR_model_midcap   %>% Waldtest(covariance_mat_mid, R)
res_large <- SUR_model_largecap %>% Waldtest(covariance_mat_large, R)

# Find the 95% critical value
Q_small <- res_small[[1]]
Q_mid   <- res_mid[[1]]
Q_large <- res_large[[1]]
critical_value <- qchisq(1 - alpha, df = Q_small)
{print(paste("95% critical value:", critical_value))
  print(paste("Wald test statistic and p-value for restricted model (small):", res_small[2],
              " and ", 1 - pchisq(res_small[[2]], df = Q_small)))
  print(paste("Wald test statistic and p-value for restricted model (mid):  ", res_mid[2],
              " and ", 1 - pchisq(res_mid[[2]], df = Q_mid)))
  print(paste("Wald test statistic and p-value for restricted model (large):", res_large[2],
              " and ", 1 - pchisq(res_large[[2]], df = Q_large)))}
