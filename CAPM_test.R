################################################################################
#                                                                              #
# This code downloads historical closing prices for 30 stocks in the period    #
# from 2002-01-01 to 2022-01-0. Here 10 stocks are chosen from a small cap     #
# stocks list, 10 from a mid cap stocks list, and 10 from a large cap list.    #
# Then we use these 30 stocks to test the CAPM for being a restricted model    #
# using SUR model estimation and HC3.                                          #
#                                                                              #
# OVERVIEW - The script is organized into 4 sections:                          #
#   - PROGRAM PARAMETERS                                                       #
#   - LOAD FF3 FACTORS INTO R                                                  #
#   - FUNCTIONS                                                                #
#   - ANALYSIS EXECUTION                                                       #
#                                                                              #
# The ANALYSIS EXECUTION is the "main" part of of the script.                  #
#                                                                              #
# BEFORE RUNNING Program:                                                      #
#   1) Make sure to edit path on line 50 such that program can find the Fama-  #
#      French factors.                                                         #
#   2) Edit line 53 to TRUE if plots are wanted. (They take time)              #
#                                                                              #
################################################################################
rm(list = ls())
set.seed(100) # Sets seed. Should be 100 to produce same results as
              # given in the project
library(yahoofinancer)
library(magrittr)
library(ggplot2)
library(cowplot)
library(tseries)
library(moments)
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
PATH <- "C:\\Users\\janre\\Documents\\uni\\7. Semester\\Projekt\\Kode\\factors.csv"

# Parameters:
doPlots  <- TRUE # If TRUE, the program makes the plots
alpha    <- 0.05  # Confidence level

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

# Takes a vector of simple excess return, fits a CAPM, and plots a density 
# histogram and qqplot of the standardized residuals, to visualize the 
# distribution of the standardized residuals.
histQQNormPlot <- function(data, name) {
  # First fit a CAPM, and calculate std. residuals
  model <- lm(data ~ factors$Mkt.RF)
  residuals <- model %>% resid %>% scale %>% {data.frame(x = .)}
  residuals %<>% na.omit 
  
  # Create the histogram plot
  hist_plot <- ggplot(residuals, aes(x = x)) +
    theme_minimal() +
    geom_histogram(aes(y = after_stat(density)), 
                   binwidth = 0.1, fill = "blue", 
                   color = "black", 
                   alpha = 0.7) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                  color = "red", size = 1) +
    labs(subtitle = paste("Skewness:", residuals %>% skewness %>% round(digits = 4)),
         x = "Standardized residuals",
         y = "Frequency")
  
  # Create the QQ plot
  qq_plot <- ggplot(residuals, aes(sample = x)) +
    theme_minimal() +
    stat_qq() +
    stat_qq_line() +
    labs(subtitle = paste("Kurtosis:", residuals %>% kurtosis %>% round(digits = 4)),
         x = "Theoretical quantiles",
         y = "Sample quantiles")
  
  combined_plot <- (hist_plot + qq_plot)
  title_text <- ggdraw() + draw_label(name, size = 16, hjust = 0.5)
  
  # Combine both plots
  plot_grid(title_text, combined_plot, ncol = 1, rel_heights = c(0.1, 1)) %>% return
}

# Takes a data frame of excess returns. Fits model on all
# excess returns columns and check for normally distributed
# residuals with the Jarque-Bera test.
testNormalityModels <- function(data) {
  vals <- 0
  for(col in names(data[, -1])) {
    # Fit a CAPM, and calculate std. residuals, find
    # the p-value from the jarque-bera test
    model <- lm(data[[col]] ~ factors$Mkt.RF)
    res <- model %>% resid %>% scale %>% jarque.bera.test %$% p.value
    if(res >= 0.05) {
      vals <- vals + 1 
    }
  }
  # Returns acceptance rate (number of accepted jarque-bera tests /
  # number af tests performed)
  return(vals / length(data[, -1]))
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

# Takes a dataframe of stock returns, and creates a new list where for each
# stock there is a column of Mkt.RF with only the values that corresponds to
# the available stock returns.
aligndataframes <- function(stocks) {
  # Aligning returns with factors, so we have no NA-values, and factors are
  # aligned with the correct stock return values
  liste <- list()
  for(name in names(stocks[, -1])) {
    values <- data.frame(stocks[[name]], f = factors$Mkt.RF)
    values %<>% na.omit
    liste[[name]] <- data.frame(values$f)
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

## Test for normality - Before we do anything, we would like to know, if our
# data are normally distributed.
{smallcap_return %>% testNormalityModels %>% print
midcap_return    %>% testNormalityModels %>% print
largecap_return  %>% testNormalityModels %>% print}

# They are all zero, suggesting that none of our residuals are normally 
# distributed. We supplement this result with a visual inspection on the 
# residuals for some of the CAPMs.

# Plotting histogram and qqplot for 3 stocks, one for each size.
# Do this 3 times and concatenate to one big plot
if(doPlots) {
  col1 <- smallcap_return$RES %>% histQQNormPlot("RES") /
    midcap_return$AMBA %>% histQQNormPlot("AMBA") /
    largecap_return$AAPL %>% histQQNormPlot("AAPL")
  
  col2 <- smallcap_return$FCPT %>% histQQNormPlot("FCPT") /
    midcap_return$CLOV %>% histQQNormPlot("CLOV")   /
    largecap_return$MSFT %>% histQQNormPlot("MSFT")
  
  col3 <- smallcap_return$SIX %>% histQQNormPlot("SIX") /
    midcap_return$SFIX %>% histQQNormPlot("SFIX") /
    largecap_return$GOOG %>% histQQNormPlot("GOOG")
  
  combined_plots <- plot_grid(col1, col2, col3, ncol = 3)
  combined_plots %>% print
  rm(combined_plots, col1, col2, col3)
}

## Fit a SUR model for smallcap, one for midcap, one for largecap, and one 
# for all of them combined. Perform a Wald test to test if the intercepts are
# significant.

# For small cap
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

# For all combined
combined <- cbind(smallcap_return, midcap_return[-1], largecap_return[-1])
y <- listifyStocks(combined)
x <- aligndataframes(combined)
SUR_model_combined <- surfit(y, x)

# Setup the restrictions for the models
R      <- diag(1, nrow = 20, ncol = 20)
R_comb <- diag(1, nrow = 60, ncol = 60)
R[seq(2, 20, by = 2), seq(2, 20, by = 2)] <- 0
R_comb[seq(2, 60, by = 2), seq(2, 60, by = 2)] <- 0

# Perform a Wald test to test if the intercepts are significant
res_small <- SUR_model_smallcap %>% Waldtest(vcovHC(., type = "HC3"), R)
res_mid   <- SUR_model_midcap   %>% Waldtest(vcovHC(., type = "HC3"), R)
res_large <- SUR_model_largecap %>% Waldtest(vcovHC(., type = "HC3"), R)
res_combined <- SUR_model_combined %>% Waldtest(vcovHC(., type = "HC3"), R_comb)
  
# Calculate the 95% confidence region. The Wald statistic follow as chi
# squared distribution with Q degrees of freedom under the null hypothesis
Q_small <- res_small[[1]]
Q_mid   <- res_mid[[1]]
Q_large <- res_large[[1]]
Q_combined <- res_combined[[1]]

confidence_level <- 1 - alpha
lower_critical_value <- qchisq((1 - confidence_level) / 2, df = Q_small)
upper_critical_value <- qchisq(1 - (1 - confidence_level) / 2, df = Q_small)
confidence_interval <- c(lower_critical_value, upper_critical_value)
{print(paste("95% Confidence region:", confidence_interval[1], 
            confidence_interval[2]))
print(paste("Wald test statistic for restricted model (small):   ", res_small[2]))
print(paste("Wald test statistic for restricted model (mid):     ", res_mid[2]))
print(paste("Wald test statistic for restricted model (large):   ", res_large[2]))}

# Confidence region for the combined model
lower_critical_value <- qchisq((1 - confidence_level) / 2, df = Q_combined)
upper_critical_value <- qchisq(1 - (1 - confidence_level) / 2, df = Q_combined)
confidence_interval <- c(lower_critical_value, upper_critical_value)
{print(paste("95% Confidence region for combined model:", confidence_interval[1], 
             confidence_interval[2]))
print(paste("Wald test statistic for restricted model (combined):   ", res_combined[2]))}