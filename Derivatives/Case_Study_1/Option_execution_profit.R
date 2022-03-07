library(highfrequency)
library(xts)
library(quantmod)

###Payoff of the put warrant when executed
#load and prepare historical data of Nikkei 225
data <- read.csv("https://raw.githubusercontent.com/SweetyHotGirl21/Derivatives/main/nikkei-225-index-historical-chart-data.csv", sep = ",", dec = ".")
data$Date <- as.Date(data$Date, foramt = "%Y-%m-%d")
#Select period of interest
Nikkei <- data[as.Date("1992-12-20") > data$Date & data$Date > as.Date("1989-12-18"),]

K <- 38686.18
FX <- 144.28
share <- 0.2

payoff <- round(share * (K - Nikkei$Nikkei255)/FX,4)

option.ts <- as.xts(data.frame(Nikkei$Nikkei255,payoff),order.by <- Nikkei[,1])

chartSeries(option.ts[,2],
            TA = 'addTA(option.ts[,1], legend = "NIKKEI 225")',
            name = "Nikkei warrent put exercise payoff"
)

dates <- c("1990-12-19","1991-12-19","1992-12-18")
addLines(v = c(which(Nikkei$Date %in% as.Date(dates,foramt="%Y-%m-%d"))))

for(i in dates){
  d <- c(which(Nikkei$Date %in% as.Date(i,foramt="%Y-%m-%d")))
  cat("The payoff in", i , "is", payoff[d],"$. ")
}

