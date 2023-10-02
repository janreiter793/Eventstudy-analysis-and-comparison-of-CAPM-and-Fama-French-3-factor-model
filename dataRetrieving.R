library(yahoofinancer)
library(magrittr)
library(zoo)
library(tidyr)

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

########## ANALYSIS EXECUTION ##########
# Check if all companies are listed on yahoo finance
c(names_LCAP, names_MCAP, names_SCAP) %>% isListed

smallcap_prices <- retrievePrices(PERIOD, names_SCAP)
midcap_prices <- retrievePrices(PERIOD, names_MCAP)
largecap_prices <- retrievePrices(PERIOD, names_LCAP)
smallcap_prices %>% tail
midcap_prices   %>% tail
largecap_prices %>% tail

smallcap_return <- smallcap_prices %>% df_priceToSimpleReturn
midcap_return   <- midcap_prices %>% df_priceToSimpleReturn
largecap_return <- largecap_prices %>% df_priceToSimpleReturn
smallcap_return %>% tail
midcap_return   %>% tail
largecap_return %>% tail

# Read the factors Rm - Rf, SMB, HML, Rf
csv <- read.csv(PATH)

# Remove last row, change date format
csv %<>% {.[-nrow(.), ]}
csv$Date %<>% as.Date(format = "%Y%m%d")

# Only take the subset of the factors that fall in the given period
csv %<>%  {.[.$Date >= as.Date(PERIOD[1]) & .$Date <= as.Date(PERIOD[2]), ]}

factors <- PERIOD %>% as.Date %>% {seq(.[1], .[2], "days")} %>% data.frame(Date = .)
factors %<>% merge(csv, all.x = TRUE) 
for(factor in names(factors[, -c(1)])) {
  factors[[factor]] %<>% na.approx(na.rm = FALSE)
  factors[[factor]] %<>% na.locf(na.rm = FALSE)
  factors[[factor]] %<>% rev %>% na.locf(na.rm = FALSE) %>% rev
}
factors %>% tail

########## PROGRAM END ##########
#rm(list = ls())
