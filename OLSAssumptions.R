#
# OVERVIEW - The script is organized into 4 sections:
#   - PROGRAM PARAMETERS
#   - LOAD FF3 FACTORS INTO R
#   - FUNCTIONS
#   - ANALYSIS EXECUTION
#
# The ANALYSIS EXECUTION is the "main" part of of the script.
#
# BEFORE RUNNING Program: Make sure to edit path on line 40 such that
#                         program can find the Fama-French factors
#
rm(list = ls())
library(yahoofinancer)
library(magrittr)
library(zoo)
library(caret)
library(tseries)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)

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

# Takes a vector of simple excess return, fits an FF-3-factor model,
# and plots a density histogram and qqplot of the standardized 
# residuals, to visualize the distribution of the standardized 
# residuals.
histQQNormPlot <- function(data, name) {
  # First fit an FF-3-factor model, and calculate std. residuals
  model <- lm(data ~ factors$Mkt.RF + factors$SMB + factors$HML)
  residuals <- model %>% resid %>% scale %>% {data.frame(x = .)}
  
  # Create the histogram plot
  hist_plot <- ggplot(residuals, aes(x = x)) +
    theme_minimal() +
    geom_histogram(aes(y = ..density..), 
                   binwidth = 0.1, fill = "blue", 
                   color = "black", 
                   alpha = 0.7) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                  color = "red", size = 1) +
    labs(title = name,
         subtitle = "Histogram of Residuals",
         x = "Standardized residuals",
         y = "Frequency")
  
  # Create the QQ plot
  qq_plot <- ggplot(residuals, aes(sample = x)) +
    theme_minimal() +
    stat_qq() +
    stat_qq_line() +
    labs(subtitle = "Standard qq-plot",
         x = "Sample quantiles",
         y = "Theoretical quantiles")
  
  # Combine both plots
  (hist_plot + qq_plot) %>% return
}

# Takes a data frame of excess returns. Fits model on all
# excess returns columns and check for normally distributed
# residuals with the jarque-bera test.
testNormalityModels <- function(data) {
  vals <- 0
  for(col in names(data[, -1])) {
    # Fit an FF-3-factor model, and calculate std. residuals, find
    # the p-value from the jarque-bera test
    model <- lm(data[[col]] ~ factors$Mkt.RF + factors$SMB + factors$HML)
    res <- model %>% resid %>% scale %>% jarque.bera.test %$% p.value
    if(res >= 0.05) {
      vals <- vals + 1 
    }
  }
  # Returns acceptance rate (number of accepted jarque-bera tests /
  # number af tests performed)
  return(vals / length(data[, -1]))
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

# Average acceptance rates for ff-3-factor model on small,
# medium, and large cap stock returns.
smallcap_return %>% testNormalityModels
midcap_return   %>% testNormalityModels 
largecap_return %>% testNormalityModels

# Plotting histogram and qqplot for 3 stocks, one for each size.
# Do this 3 times and concatenate to one big plot
col1 <- smallcap_return$RES %>% histQQNormPlot("RES (S. cap)") /
midcap_return$AMBA %>% histQQNormPlot("AMBA (M. cap)") /
largecap_return$AAPL %>% histQQNormPlot("AAPL (L. cap)")
  
col2 <- smallcap_return$FCPT %>% histQQNormPlot("FCPT (S. cap)") /
midcap_return$CLOV %>% histQQNormPlot("CLOV (M. cap)")   /
largecap_return$MSFT %>% histQQNormPlot("MSFT (L. cap)")

col3 <- smallcap_return$SIX %>% histQQNormPlot("SIX (S. cap)") /
midcap_return$SFIX %>% histQQNormPlot("SFIX (M. cap)") /
largecap_return$GOOG %>% histQQNormPlot("GOOG (L. cap)")

combined_plots <- plot_grid(col1, col2, col3, ncol = 3)
combined_plots %>% print
