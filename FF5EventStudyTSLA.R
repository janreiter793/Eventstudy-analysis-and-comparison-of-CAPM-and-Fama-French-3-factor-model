#
# OVERVIEW - The script is organized into 4 sections:
#   - LOAD FF3 FACTORS INTO R
#   - FUNCTIONS TO RUN THE PROGRAM
#   - GETTING THE EXCESS RETURNS FOR THE TSLA STOCK
#   - PERFORMING THE EVENT STUDY
#
# PERFORMING THE EVENT STUDY is the "main" part of of the script.
#
# BEFORE RUNNING Program: Make sure to edit path on line 28 such that
#                         program can find the Fama-French factors
#
rm(list = ls())
library(yahoofinancer)
library(magrittr)
# library(zoo)
# library(caret)
# library(boot)
# library(latex2exp)
# library(fitdistrplus)

########## PROGRAM PARAMETERS ##########
# Period to retrieve data from
PERIOD <- c('2002-01-01', '2023-10-31')

# Path to the FF-3 factors
# Remember to fill in path to the file containing the factors
PATH <- "C:\\Users\\Krist\\OneDrive - Aalborg Universitet\\AAU\\Semester 7\\project\\EventStudy\\FF5Factors301123.CSV"

########## LOAD FF3 FACTORS INTO R ##########
# Read the factors Rm - Rf, SMB, HML, Rf
csv <- read.csv(PATH)

# Change date format
csv$Date %<>% as.character() %>%  as.Date(format = "%Y%m%d")

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

# Takes a vector of prices and turn it into simple returns
priceToSimpleReturn <- function(prices) {
    prices %>% {diff(.) / .[1:(length(.) - 1)]} %>% return
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
    
    # Making the stock prices to simple returns
    data$close %<>% {c(NA, priceToSimpleReturn(.))} 
    
    # Merge stockprices and data. Data will have fewer rows than stock prices,
    # because of missing dates. Values for these missing dates will be filled
    # with NA's
    stockprices %<>% merge(data, all.x = TRUE)
    
    # Change name of column to the given stock
    colnames(stockprices)[ncol(stockprices)] <- name
    
    return(stockprices)
}


# Takes a data frame of stock returns (with a date column) and turns
# the returns into excess returns with respect to RF
returnsToExcessReturn <- function(returns) {
    for(col in names(returns[, -1])) {
        returns[[col]] %<>% {. - factors$RF}
    }
    return(returns)
}


########## Getting the excess returns ##########

# The TSLA stock is used
Tesla <- 'TSLA'

# Check if correct name and viable
isListed(Tesla)

# Finding the fist date which price is not NA
vec <- retrievePrices(PERIOD, Tesla)

# Finding the rows with prices bein NA
non_na_vec <- which(!is.na(vec)[,2]) 
# Determining the minimum from these indices 
first_non_na <- min(non_na_vec) 

# Updating the period to start fron the first date with a price
PERIOD <- c(vec[first_non_na,1], '2023-10-31')

rm(vec, non_na_vec, first_non_na)

# Updating the factors such that they align in start date
factors <- factors[factors$Date >= PERIOD[1] & factors$Date <= PERIOD[2], ]

# Getting the returns for the trading days
TeslaPrices <- retrievePrices(PERIOD, Tesla)

# Making the prices to simple excess returns by subtracting the risk free rate
TeslaPrices$TSLA <- TeslaPrices$TSLA - factors$RF

# Getting the dates for each price
dates <- TeslaPrices$date[!is.na(TeslaPrices[ , 2])]


# Extracting the returns on day which it is not NA,
# we have NA on non-trading days, 
# since simple return can't be calculated day to day on those days
values_stock <- TeslaPrices[ , 2] %>% {.[!is.na(.)]}

# Extracticng the factors on days where the Tesla return in not NA
values_factors <- TeslaPrices[ , 2] %>% {factors[!is.na(.), 2:6]}

# Creating a data frame with the dates, TSLA simple excess return, 
# and the factors for the FF-3 model
data <- data.frame(date = dates, TSLAReturn = values_stock) %>% 
    cbind(values_factors)

rm(values_stock, values_factors)


########## Performing the event study ##########

# The list of events and their name
Events <- 
    list(c("2018-08-07", "Musk offers to buy back TSLA"),
         c("2018-09-07", "Musk smokes marijuana on JRE"),
         c("2019-11-21", "Cybertruck's windows crack"),
         c("2022-04-14", "Musk offers to buy Twitter"),
         c("2023-09-07", "Musk allegedly turned off Starlink"))


# Making the dates to interpretable dates for the program
EvDates <- sapply(Events, function(vec) vec[1] %>% as.Date())

# Extracting the event name
EvNames <- sapply(Events, '[', 2)

# Putting the event dates and types into a dataframe
EventDates <- data.frame(date = EvDates, EventName = EvNames)
########## Setting up the event study ##########

# Observations in the estimation window to train the model on before an event
EstimationWindowLength <- 120
EstimationWindow <- 1:EstimationWindowLength

# Number of closing prices in the event window minus 1, since we start at t = 0
EventWindowLength <- 14
EventWindow <- 0:(EventWindowLength - 1)


# Creating the dataframe which contains the abnormal returns with these columns
columns <- c("EventDate", "EventName",
             "AbnormalReturnT0", "AbnormalReturnT1", 
             "AbnormalReturnT2", "AbnormalReturnT3", 
             "AbnormalReturnT4", "AbnormalReturnT5", 
             "AbnormalReturnT6", "AbnormalReturnT7", 
             "AbnormalReturnT8", "AbnormalReturnT9", 
             "AbnormalReturnT10", "AbnormalReturnT11", 
             "AbnormalReturnT12", "AbnormalReturnT13") 

# Creating an empty DataFrame with 0 rows and 16 columns
AnalysisData <- data.frame(matrix(nrow = 0, ncol = length(columns)),
                           stringsAsFactors = FALSE) 
# Assign column names
colnames(AnalysisData) = columns

# Making a list to save the residuals
residualsList <- list()


# For loop to get the normal returns for each event and the model residuals
for (i in which(data$date %in% intersect(EventDates$date, data$date)) ) {

    # Creating the model on the dates in the Estimation Window
    FF5model <- lm(TSLAReturn ~ . -date, 
                   data = data[i-EstimationWindow, ])
    print(data[i-EstimationWindow,1])
    # Extracting the residuals from the model
    resids <- FF5model$residuals
    
    # Save the residuals to the list
    residualsList[[paste(EventDates[which(EventDates$date %in% data[i,]$date), 
                                    "EventName"])]] <- resids
    
    # Calculating the normal return during the Event Window
    NormalReturns <- predict(FF5model, 
                             newdata = data[i+EventWindow,])
    
    # Calculating the abnormal returns
    AR <- data[i+EventWindow, 'TSLAReturn'] - NormalReturns %>% unname()
    
    # Creating the row to be added to AnalysisData for the 
    # current event investigated
    new_row <- data.frame(
        EventDate = data[i, 'date'],
        EventName = EventDates[which(EventDates$date %in% data[i,]$date), "EventName"],
        AbnormalReturnT0 = AR[1],
        AbnormalReturnT1 = AR[2],
        AbnormalReturnT2 = AR[3],
        AbnormalReturnT3 = AR[4],
        AbnormalReturnT4 = AR[5],
        AbnormalReturnT5 = AR[6],
        AbnormalReturnT6 = AR[7],
        AbnormalReturnT7 = AR[8],
        AbnormalReturnT8 = AR[9],
        AbnormalReturnT9 = AR[10],
        AbnormalReturnT10 = AR[11],
        AbnormalReturnT11 = AR[12],
        AbnormalReturnT12 = AR[13],
        AbnormalReturnT13 = AR[14]
    )
    # Adding the row to AnalysisData
    AnalysisData <- rbind(AnalysisData, new_row)
    
}
AnalysisData


# Creating the dataframe which contains the test statistics with these columns
columns <- c("Event", "CAR",
             "Standard.deviation", "t.statistic", 
             "p.value") 

# Creating an empty DataFrame with 0 rows and 5 columns
AbnormalData <- data.frame(matrix(nrow = 0, ncol = length(columns)),
                           stringsAsFactors = FALSE) 
# Assign column names
colnames(AbnormalData) = columns

# For loop for getting abnormal returns, CAR, test statistics, and p-values
for (i in 1:nrow(AnalysisData)) {
    
    # Calculating the CAR for the event window
    CAR <- sum(AnalysisData[i,3:4])
    
    # Calculating the unbiased estimator of the standard deviation
    ARsd <- 1/(EstimationWindowLength - 4) * 
        sum((as.numeric(unlist(unname(residualsList[AnalysisData[i,2]]))))^2)
    
    # Calculating the test statistic t_CAR
    tCAR <- CAR / (sqrt(2 * ARsd))
    
    # Calculating the p-value for the test statistic
    pValue <- 2 *min(pt(tCAR, df = EstimationWindowLength - 4), 
                     1 - pt(tCAR, df = EstimationWindowLength - 4)) 
    
    
    # Creating the row to be added to AbnormalData for the 
    # current event investigated
    new_row <- data.frame(
        Event = AnalysisData[i, 2],
        CAR = CAR,
        Standard.deviation = ARsd,
        t.statistic = tCAR,
        p.value = pValue
    )
    
    # Adding the row to AbnormalData
    AbnormalData <- rbind(AbnormalData, new_row)
    
}

# The dataframe containing the values for the tests of the events
AbnormalData

# Creating where to put breaks in the histograms
breaks <- seq(from = -50, to = 50, by = 0.01)

# Creating the residual plots along with their kernel densities
for (i in 1:nrow(AnalysisData)) {
    as.numeric(unlist(unname(residualsList[AnalysisData[i,2]]))) %>% 
        hist(breaks = breaks, freq = FALSE, 
             main = paste("Histogram for the event ", 
                          AnalysisData[i, 2], "", sep = "\""),
             xlab = "Residuals",
             xlim = c(-0.20, 0.20), xaxt = "s", xaxp = c(-0.2, 0.2, 40),
             ylim = c(0, 25),
             cex.main = 1.45, cex.lab = 1.45, cex.axis = 1,
             col = "lightblue"); grid()
    
    as.numeric(unlist(unname(residualsList[AnalysisData[i,2]]))) %>%
        density(adjust = 1, kernel = "gaussian") %>% 
        lines(type = "l", lty = "solid", col = "red", lwd = 3)
}




# The y-placement for the observation numbers in the following plots
valuesHeight <- list(-c(1, 1, 1, 1, 1, 1, 1, 1.75, 1, 1.75, 1, 1.75, 1, 2.5),
                     -c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1.75, 1.75),
                     -c(1, 1, 1, 1, 1, 1.75, 1, 1, 1, 1.75, 2.5, 1.75, 1.75, 1.75),
                     -c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1.75, 1.75, 2.5, 1.75),
                     -c(1, 1, 1, 1, 1, 1, 1, 1, 1.75, 2.5, 1.75, 1, 1.75, 1))


# Constructing the plots with kernel densities and observations after the event
for (i in 1:nrow(AnalysisData)) {
    
    plot(density(as.numeric(unlist(unname(residualsList[AnalysisData[i,2]])))), 
         main = paste("Density Plot for the event ", 
                      AnalysisData[i, 2], "", sep = "\""),
         xlab = "Residual", xaxp = c(-.2,.2, 16),
         ylab = "Density", ylim = c(-2, 22),
         cex.main = 2.5, cex.lab = 1.5, cex.axis = 1.5)
    
    par(xaxp = c(-.2,.2, 16))
    grid()
    
    
    median <- median(as.numeric(unlist(unname(residualsList[AnalysisData[i,2]]))))
    
    abline(v = median, col = "blue", lty = 2)
    
    values <- AnalysisData[i, 3:16] %>% unname()
    
    for (j in seq_along(values)) {
        alpha <- 1 - (j / (length(values) + 1))  # Calculate alpha for fading effect
        lines(c(values[j], values[j]), c(0, 2),
              col = rgb(1, 0, 0, alpha = alpha), lty = 1, lwd = 3)
    }
    
    text(values, valuesHeight[[i]], 
         labels = sprintf("%d", 1:14),#ecdf(resids)(numbers)),
         pos = 3, col = "red", cex = 1.5)
    
    legend("topright", inset = 0.02, legend = "Median",
           col = "blue", lty = 2, cex = 2,
           box.lty = 0)
}


library(xtable)

xtable(AbnormalData, type = "latex")

