### This script is the basis
# It should be used to load the relevant packages and data for the Analysis done in
#     June_20200815_Stock-Components, where the test of Faram and French are applied to my data
#     June_20200815_Variance-Ratio-Test, where the variance ratio test of Lo & MacKinlay is apllied to my data
remove(list = ls())

library(forecast) #Acf and Pacf
library(highfrequency)
library(xts)
library(zoo)
library(quantmod)
library(lubridate) #adjusting timezone
library(ggplot2)
library(dyn) #lag operator regression
library(tseries) #kpss Test #GARCH
library(moments) #skwness, kurtosis
library(vrtest)
library(fGarch)
library(FinTS)

###### Set local working directory where the data file is located
setwd("C:/Users/B-C-Herbert/Documents/Studium/Mannheim/VWL/Thesis/Data/")

### Read data as dataframe 
df <- read.csv(file = "intraday_prices_June_clean.csv", header = TRUE, sep = ";", dec = ","); head(df)
### Dataframe contains 5-min prices and volumes of 5 stocks as well as a Timestamp
### clean data
df <- subset(df, select = -c(X, X.1,X.2, X.3))
df <- na.omit(df); head(df); tail(df)
### Creat timestamp in POISXct format and add to dataframe
Timestamp <- as.POSIXct(df$Timestamp, format = "%d-%m-%Y %H:%M")
head(Timestamp); tail(Timestamp)
Timestamp <- with_tz(Timestamp, tzone = "America/New_York")
head(Timestamp); tail(Timestamp)
df <- data.frame(Timestamp, subset(df, select = -Timestamp ))
### Order dataframe (unsig dates) with newest observation first
DF <- df[order(df$Timestamp),]; head(DF)

### Split dataframe into different stocks
### calculation of first log-differance from prices (returns)
### Cutting dataframe to relevant timespan 01.06.2020 - 26.06.2020 (1738 obervations / variable)

#Alphabet
Google <- data.frame(DF[,1:3], Google_Return = c(NA, diff(log(DF[,3]))))
Google.ts <- as.xts(Google[,2:4], order.by <- Google[,1])
Google.ts <- Google.ts[index(Google.ts) < as.POSIXct("2020-06-27") & index(Google.ts) > as.POSIXct("2020-06-01")]
head(Google.ts) # Warning message is not relevant
tail(Google.ts) # Warning message is not relevant
Google.summary <- summary(Google.ts); Google.summary
Google.sd <- sd(Google.ts[,3]); Google.sd
Google.skrw <- skewness(Google.ts[,3])[1]; Google.skrw
Google.kurt <- kurtosis(Google.ts[,3])[1]; Google.kurt

# Goldman Sachs
GS <- data.frame(Timestamp = DF$Timestamp, DF[,4:5], GS_Return = c(NA, diff(log(DF[,5]))))
GS.ts <- as.xts(GS[,2:4], order.by <- GS[,1])
GS.ts <- GS.ts[index(GS.ts) < as.POSIXct("2020-06-27") & index(GS.ts) > as.POSIXct("2020-06-01")]
head(GS.ts)
tail(GS.ts)
GS.summary <- summary(GS.ts); GS.summary
GS.sd <- sd(GS.ts$GS_Return); GS.sd
GS.skew <- skewness(GS.ts[,3])[1]; GS.skew #-3.4621
GS.kurt <- kurtosis(GS.ts[,3])[1]; GS.kurt #63.0850


# Procter & Gamble
PuG <- data.frame(Timestamp = DF$Timestamp, DF[,6:7], PuG_Return = c(NA, diff(log(DF[,7]))))
PuG.ts <- as.xts(PuG[,2:4], order.by = PuG[,1])
PuG.ts <- PuG.ts[index(PuG.ts) < as.POSIXct("2020-06-27") & index(PuG.ts) > as.POSIXct("2020-06-01")]
head(PuG.ts)
tail(PuG.ts)
PuG.summary <- summary(PuG.ts); PuG.summary
PuG.sd <- sd(PuG.ts$PuG_Return); PuG.sd
PuG.skew <- skewness(PuG.ts[,3])[1]; PuG.skew #0.1804
PuG.kurt <- kurtosis(PuG.ts[,3])[1]; PuG.kurt #13.7952

# Netflix
NFLX <- data.frame(Timestamp = DF$Timestamp, DF[,8:9], NFLX_Return = c(NA, diff(log(DF[,9]))))
NFLX.ts <- as.xts(NFLX[,2:4], order.by <- NFLX[,1])
NFLX.ts <- NFLX.ts[index(NFLX.ts) < as.POSIXct("2020-06-27") & index(NFLX.ts) > as.POSIXct("2020-06-01")]
head(NFLX.ts)
tail(NFLX.ts)
NFLX.summary <- summary(NFLX.ts); NFLX.summary
NFLx.sd <- sd(NFLX.ts$NFLX_Return); NFLx.sd
NFLX.skew <- skewness(NFLX.ts[,3])[1]; NFLX.skew  #0.2982
NFLX.kurt <- kurtosis(NFLX.ts[,3])[1]; NFLX.kurt #6.2806

### descriptiv summary
data.summary <- data.frame(No.observations = c(1580, 1580, 1580, 1580),mean = c(Google.summary[4,4], GS.summary[4,4], PuG.summary[4,4], NFLX.summary[4,4]),
                          Minimum = c(Google.summary[1,4], GS.summary[1,4], PuG.summary[1,4], NFLX.summary[1,4]),
                          Maximum = c(Google.summary[6,4], GS.summary[6,4], PuG.summary[6,4], NFLX.summary[6,4]), 
                          St.deviation = c(Google.sd, GS.sd, PuG.sd, NFLx.sd), 
                          skewness = c(Google.skrw, GS.skew, PuG.skew, NFLX.skew),
                          Kurtosis = c(Google.kurt, GS.kurt, PuG.kurt, NFLX.kurt)) 
row.names(data.summary) <- c("Alphabet", "Goldman", "PuG", "Netflix"); data.summary


### Graphs of returns and price porces
chartSeries(Google.ts[,3],
            TA = 'addTA(Google.ts[,2], legend = "Intraday Prices")',
            name = "Alphabet Intraday Returns"
)
chartSeries(GS.ts[,3],
            TA = 'addTA(GS.ts[,2], legend = "Intraday Prices")',
            name = "Goldman Sachs Intraday Returns"
)
chartSeries(PuG.ts[,3],
            TA = 'addTA(PuG.ts[,2], legend = "Intraday Prices")',
            name = "Procter & Gamble Intraday Returns"
)
chartSeries(NFLX.ts[,3],
            TA = 'addTA(NFLX.ts[,2], legend = "Intraday Prices")',
            name = "Netflix Intraday Returns"
)




Acf(Google.ts$Google_Return, lag.max = 79)
Pacf(Google.ts$Google_Return, lag.max = 79)
Acf(GS.ts$GS_Return)
Acf(PuG.ts$PuG_Return)
Acf(NFLX.ts$NFLX_Return)

### Estimating ARMA models of returns
Google.ts.arima <- auto.arima(Google.ts$Google_Return)
GS.ts.arima <- auto.arima(GS.ts$GS_Return)
PuG.ts.arima <- auto.arima(PuG.ts$PuG_Return)
NFLX.ts.arima <- auto.arima(NFLX.ts$NFLX_Return)

Google.ts.arima$coef
autoplot(Google.ts.arima$x)+
  autolayer(Google.ts.arima$residuals)+
  autolayer(Google.ts.arima$fitted)
Acf(Google.ts.arima$residuals, lag.max = 79)
Acf(Google.ts.arima$fitted, lag.max = 79)

Acf(Google.ts$Google_Return, lag.max = 1580)


Acf(PuG.ts$PuG_Return)
autoplot(PuG.ts.arima$residuals)
Acf(PuG.ts.arima$residuals, lag.max = 40)
Acf(PuG.ts.arima$fitted, lag.max = 30)



