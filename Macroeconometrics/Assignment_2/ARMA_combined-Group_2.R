library(forecast)
library(ggplot2)
library(ggpubr) #install.packages("ggpuhr")
library(fma)
library(expsmooth)
library(fpp2)
library(tseries) #install.packages("tseries")
library(scales)
library(stargazer) #install.packages("stargazer")
library(urca) #install.packages("urca")
library(GGally) #install.packages("GGally")
library(caschrono)

remove(list =ls())

###### Reading Data 
#Convert data to ts
Data = read.csv(file = "fin_ameco.csv", header = TRUE, sep = ";", dec = "," )
Data = Data[0:50,]
GDP = ts((Data[,2]), frequency = 1, start = c(1970), end = c(2019))

train = diff(log(GDP), lag =1)
#train = GDP
test1 = ts(c(0.015), frequency = 1, start = c(2020), end = c(2020))
True_Val = ts(c(-0.035), frequency = 1, start = c(2020), end = c(2020))
#test = ts(DATA[57:60,2], frequency = 1, start = c(2020), end = c(2023))

summary(train)
autoplot(train, series = "GDP Finland (US$ 2010)")+
  ggtitle("Trainingsset GDP Finland (US$ 2010) 1960 - 2019")+
  xlab("Year")+
  ylab("Value per Year")

#Parctial Autocorrelation plot for GDP
Acf = ggAcf(train, lag.max = 20)+
  ggtitle("Autocorrelation function GDP Finland")
#Parctial Autocorrelation plot for GDP
Pacf = ggPacf(train, lag.max = 20)+
  ggtitle("Partial Autocorrelation function GDP Finland")
#Arranging ACF and PACF plots for income
ggarrange(Acf, Pacf, ncol = 2, nrow=1, legend = NULL,
          font.label = list(size = 14))

#Test for stationarity
kpss.test(train) 
adf.test(train)

train.arima = auto.arima(train,
                         d = 0,
                         stepwise = FALSE,
                         approximation = FALSE,
                         stationary = TRUE,
                         seasonal = FALSE)
summary(train.arima)

train.res = train.arima$residuals 

checkresiduals(train.arima)

Acf_res = ggAcf(train.res, lag.max = 20)+
  ggtitle("Autocorrelation function Residuals")
#Parctial Autocorrelation plot for GDP
Pacf_res = ggPacf(train.res, lag.max = 20)+
  ggtitle("Partial Autocorrelation function Residuals")
#Arranging ACF and PACF plots for income
ggarrange(Acf_res, Pacf_res, ncol = 2, nrow=1, legend = NULL,
          font.label = list(size = 14))

#Box Pierce test
Box.test(train.arima$residuals, lag = 10, fitdf = 0)
#Box-Ljung test
Box.test(train.arima$residuals, lag = 10, fitdf = 0, type = "Lj")
#Shapiro-Wilk test for residual normality
shapiro.test(train.arima$residuals)


train.arima.fore = forecast(train.arima, h = 1)
autoplot(window(train, start = c(2000)), series = "Training Data")+
  autolayer(train.arima.fore, series = "Forecast from ARIMA(0,0,1)")+
  autolayer(window(train.arima$fitted, start = c(2000)), series = "Fitted values from ARIMA(0,0,1)")+
  autolayer(test1, series = "OECD Forecast 2020")+
  autolayer(test2, series = "Actual growth 2020")+
  ggtitle("Forecasting GDP with ARIMA(0,0,1)")+ scale_y_continuous(name="GDP")+xlab("Year")

summary(train.arima.fore)
train.arima.fore.acc.OECD.fore = accuracy(test1, True_Val)
train.arima.fore.acc.act = accuracy(train.arima.fore, True_Val)



GDP.FORE.ACC = as.data.frame(matrix(cbind(train.arima.fore.acc.OECD.fore[1,1:6], train.arima.fore.acc.act[2,1:6]), 2, 6, byrow = TRUE),row.names = c("OECD Forecast", "AMRA(0,1) Forecast"))
colnames(GDP.FORE.ACC) = colnames(train.arima.fore.acc.act)[1:6]
GDP.FORE.ACC

NNAR = nnetar(train, p = 6, size = 4)
forecast(NNAR, h = 1)
summary(NNAR)
