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
Tesla <- 'TSLA'

# Path to the FF-3 factors
PATH <- "C:\\Users\\Krist\\OneDrive - Aalborg Universitet\\AAU\\Semester 7\\project\\EventStudy\\F-F_Research_Data_Factors_daily.CSV"

########## LOAD FF3 FACTORS INTO R ##########
# Read the factors Rm - Rf, SMB, HML, Rf
csv <- read.csv(PATH)

# Remove last row, and change date format
csv %<>% {.[-nrow(.), ]}
csv$Date %<>% as.Date(format = "%Y%m%d")

# Only take the subset of the factors that fall in the given period
csv %<>%  {.[.$Date >= as.Date(PERIOD[1]) & .$Date <= as.Date(PERIOD[2]), ]}

# Create a new data frame that contains all dates in the given period
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
retrievePrices <- function(period, name) {
    # The data frame that stores all closing prices, contains all dates in the
    # period.
    stockprices <- period %>% as.Date %>% {seq(.[1], .[2], "days")} %>% data.frame(date = .)
    
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



FF3 <- function() {
    
}
# Check if correct name
isListed(Tesla)


# Finding the fist date which price is not NA
vec <- retrievePrices(PERIOD, Tesla)
non_na_vec <- which(!is.na(vec)[,2]) 
# Determining the minimum from these indices 
first_non_na <- min(non_na_vec) 

# Updating the period to start fron the first date with a price
PERIOD <- c(vec[first_non_na,1], '2022-01-01')

rm(vec, non_na_vec, first_non_na)

# Updating the factors such that they align in start date
factors <- factors[factors$Date >= PERIOD[1] & factors$Date <= PERIOD[2], ]


# Getting the closing stock price for the trading days
TeslaPrices <- retrievePrices(PERIOD, Tesla)

# Calculates the simple return for each day, 
# NA in first entry to keep same length
TeslaReturn <- TeslaPrices[,2] %>% priceToSimpleReturn %>% c(NA, .)


dates <- TeslaPrices$date[!is.na(TeslaReturn)]

# Extracting the returns on day which it is not NA
# NA on non-trading days and the days right after non-trading days,
# since simple return can't be calculated day to day on those days
values_stock <- TeslaReturn %>% {.[!is.na(.)]}

# Extracticng the factors on days where the Tesla return in not NA
values_factors <- TeslaReturn %>% {factors[!is.na(.), 2:4]}


test <- TeslaPrices[!is.na(TeslaPrices$TSLA), ]


testFactors <- factors[which(factors$Date %in% intersect(test$date, factors$Date)), ]

testSet <- test[which(test$date %in% intersect(test$date, factors$Date)), ]

head(testSet)

testSR <- testSet[,2] %>% priceToSimpleReturn %>% c(NA, .)

testdates <- testSet$date[!is.na(testSR)]


testvalues_stock <- testSR %>% {.[!is.na(.)]}

# Extracticng the factors on days where the Tesla return in not NA
testvalues_factors <- testSR %>% {testFactors[!is.na(.), 2:4]}



testdata <- data.frame(date = testdates,TSLAReturn = testvalues_stock) %>% cbind(testvalues_factors)

data <- data.frame(date = dates,TSLAReturn = values_stock) %>% cbind(values_factors)

rm(values_stock, values_factors)


########## Creating the events ##########

# The list of events and their category
Events <- 
    list(c("2020-05-05", "b"), c("2020-05-05", "a"), c("2020-05-06", "b"),
         c("2020-05-07", "y"), c("2020-05-08", "a"), c("2020-05-09", "b")
    )

# Making the dates to interpretable dates for the program
EvDates <- sapply(Events, function(vec) vec[1] %>% as.Date())

# Extracting the event type
EvTypes <- sapply(Events, '[', 2)

# Putting the event dates and types into a dataframe
EventDates <- data.frame(date = EvDates, EventType = EvTypes)

########## Setting up the event study ##########

# Observations in the estimation window to train the model on before an event
EstimationWindowLength <- 45
EstimationWindow <- 1:EstimationWindowLength

# Number of closing prices in the event window minus 1, since we start at t = 0
EventWindowLength <- 2 
EventWindow <- 0:(EventWindowLength - 1)



# Creating the dataframe which contains the abnormal returns
columns <- c("EventDate", "EventType","AbnormalReturnT0", 
             "AbnormalReturnT1","CummulativeAbnormalReturn") 

#Create a Empty DataFrame with 0 rows and n columns
AnalysisData <- data.frame(matrix(nrow = 0, ncol = length(columns)),
                           stringsAsFactors = FALSE) 
# Assign column names
colnames(AnalysisData) = columns

#TEST
AbnormalReturnsDF <- data.frame(matrix(nrow = 0, ncol = EventWindowLength))

for (i in which(data$date %in% intersect(EventDates$date, data$date)) ) {
    
    # Creatinf the model on the dates in the Estimation Window
    FF3model <- lm(TSLAReturn ~ . -date, 
                   data = testdata[i-EstimationWindow, ])
    
    # Calculating the normal return during the Event Window
    NormalReturns <- predict(FF3model, 
                             newdata = testdata[i+EventWindow,])
    
    # Calculating the abnormal returns
    AR <- testdata[i+EventWindow, 'TSLAReturn'] - NormalReturns %>% unname()
    
    # Creating the row to be added to AnalysisData for the 
    # current event investigated
    new_row <- data.frame(
        EventDate = data[i, 'date'],
        EventType = EventDates[which(EventDates$date %in% data[i,]$date), "EventType"],
        AbnormalReturnT0 = AR[1],
        AbnormalReturnT1 = AR[2],
        CummulativeAbnormalReturn = sum(AR)
    )
    # Adding the row to AnalysisData
    AnalysisData <- rbind(AnalysisData, new_row)
}    



AnalysisData[AnalysisData$EventType == "b",]
AnalysisData


ARsd <- 1/(EstimationWindowLength - 4) * sum((FF3model$residuals)^2)



bb <- AnalysisData$CummulativeAbnormalReturn / (ARsd * sqrt(EventWindowLength))

x <- seq(-5, 5, length = 100)

ahaha <- c(-200, seq(from = -50, to = 50, by = 0.2), 200)



bb %>% hist(breaks = ahaha, freq = FALSE, 
            main = TeX("Distribution of $\\widehat{\\alpha}$"),
            xlab = TeX("$\\widehat{\\alpha}$"), #col = rainbow(20),
            xlim = c(-5, 5), xaxp = c(-5, 5, 20)); grid()
curve(dnorm(x), type = "l", lty = "dashed", col = "red",lwd = 2, add = TRUE)

curve(dt(x, df = 4), 
      type = "l", lty = "dashed", lwd = 2, add = TRUE)
# Mulige problemer med datoer der bruges til hvis fx event sker fredag
# fås T+1 ikke før tirsdag, da simple returns ikke kan beregnes
# hverken lørdag, søndag eller mandag.
# Andet mulig "problem" kan være at der er forskellige tid estimations-
# vinduet varer over da der bruges et fast antal tidligere observationer,
# hvilket kan strække sig længere tilbage i tid hvis der fx er flere
# lukkedage i løbet af et vindue, samt om tweetet kommer en fredag eller mandag.
