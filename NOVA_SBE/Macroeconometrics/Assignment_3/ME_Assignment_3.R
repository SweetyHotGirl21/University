library(forecast)
library(ggplot2)
library(ggpubr) #install.packages("ggpuhr")
library(fma)
library(expsmooth)
library(fpp2)
library(seasonal)
library(mFilter) #install.packages("mFilter")
library(tseries) #install.packages("tseries")
library(scales)
library(stargazer) #install.packages("stargazer")
library(urca) #install.packages("urca")
library(GGally) #install.packages("GGally")
library(caschrono)
library(FinTS)

remove(list =ls())

DATA = read.csv(file = "Finland CPI.csv", header = TRUE, sep = ";", dec = "," )
CPI = DATA[481:793,2]

CPI = ts((CPI), frequency = 12, start = c(1995, 1), end = c(2021,1))
INF = log(CPI) %>% diff(lag=1)
INF = INF*1200



autoplot(CPI, series = "Consumer Price Index")+
  ggtitle("CPI")+
  xlab("Month")+
  ylab("Level")

autoplot(INF, series = "Inflation")+
  ggtitle("Inflation")+
  xlab("Month")+
  ylab("Level")

Acf = ggAcf(INF, lag.max = 25)+
  ggtitle("Autocorrelation function for Final consumption expenditure")

#Parctial Autocorrelation plot for consumption
Pacf = ggPacf(INF, lag.max = 25)+
  ggtitle("Partial Autocorrelation function for Final consumption expenditure")
#Arranging ACF and PACF plots for consumption
ggarrange(Acf, Pacf, ncol = 2, nrow=1, legend = NULL,
          font.label = list(size = 14))


INF.arima = auto.arima(INF, stepwise = FALSE, max.p = 12, max.q = 12,
                       approximation = FALSE, seasonal = FALSE,
                       trace = TRUE, max.order = 24 )
summary(INF.arima)

checkresiduals(INF.arima)


(fit<- Arima(INF, order=c(12,0,12),include.mean = FALSE, method = c("ML")))
checkresiduals(fit)


adf.test(CPI)

ArchTest(INF.arima$residuals)

res = INF.arima$residuals

garch(res)

install.packages('rugarch')
library('rugarch')

garch11.spec = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(12,12)))
garch11.fit = ugarchfit(spec=garch11.spec, data=INF)
show(garch11.fit)



plot(garch11.fit,which=3)



tgarch11.spec = ugarchspec(variance.model = list(model="fGARCH", submodel="TGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(12,3)))
gjrgarch11.spec = ugarchspec(variance.model = list(model="gjrGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(12,0)))

tgarch11.fit = ugarchfit(spec=gjrgarch11.spec, data=INF)
show(tgarch11.fit)

plot(tgarch11.fit,which=3)

tgarch11.fit@fit$var
plot(tgarch11.fit@fit$sigma)



autoplot(tgarch11.fit@fit$sigma)+
  ggtitle("Inflation")+
  xlab("Month")+
  ylab("Level")
