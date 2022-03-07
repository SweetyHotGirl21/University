# ________________________________________________
## Libraries
library(quantmod) #as.xts() und runSD() 
library(tseries) #adf.test() und garch()
library(forecast) #ggAcf() und ggPacf()
library(FinTS) #ArchTest()
library(rugarch) #ugarchspec()
library(fGarch) #viel Potential, siehe unten
library(highfrequency) #heavyModel()
library(ggplot2)
# ______________________________________________
## alle Funktionen

# function to convert to POSIXt
maketime <- function(csvfile) {
  a <- csvfile[,1]
  b <- strptime(a,format="%d.%m.%Y %H:%M") #defining what is the original format of your date
  csvfile[,1] <- as.POSIXct(b)
}

# function to make the data xts
process <- function(csvfile) {
  csvfile[,1] <- maketime(csvfile) #convert to POSIXt
  csvfile <- na.omit(csvfile) #take out NAs
  csvfile <- as.xts(csvfile[,2], csvfile[,1]) # convert the price data to xts
}

# function to estimate ARCH orders and GARCH(1,1)
arch.order <- function(residuals, q.max = 50) {
  arch.ic <- data.frame(p <- numeric(), q <- numeric(), N <- numeric(), LogLik <- numeric(), AIC.nc <- numeric(), AIC.wc <- numeric(), BIC <- numeric())
  colnames(arch.ic) <- c("p", "q", "N", "LogLik", "AIC", "BIC")
  N <- length(residuals)
  
  for (q in 1:q.max) {
    arch <- garch(residuals, order=c(0,q), trace = F)
    aic <- -2*logLik(arch)[1]+2*(q+1)*(N/(N-q-2))
    bic <- AIC(arch, k=log(N)) # siehe help(BIC)
    arch.ic <- rbind(arch.ic, cbind(p = 0, q, N, logLik(arch), aic, bic))
  }
  
  arch <- garch(residuals, order=c(1,1), trace=F)
  aic <- -2*logLik(arch)[1]+2*(q+1)*(N/(N-q-2))
  bic <- AIC(arch, k=log(N))
  arch.ic <- rbind(arch.ic, cbind(p = 1, q = 1, N, logLik(arch), aic, bic))
  
  aic.min <- arch.ic[arch.ic$aic == min(arch.ic$aic),]
  bic.min <- arch.ic[arch.ic$bic == min(arch.ic$bic),]
  if (aic.min[,1] == bic.min[,1]) {pvalue <<-aic.min[,1]} else {rm(p)}
  if (aic.min[,2] == bic.min[,2]) {qvalue <<- aic.min[,2]} else{rm(q)}
  
  print(arch.ic)
  print(aic.min)
  print(bic.min)
}

# function to estimate GARCH orders
garch.order <- function(residuals, max.p = 10, max.q = 10) {
  garch.ic <- data.frame(p <- numeric(), q <- numeric(), N <- numeric(), LogLik <- numeric(), AIC.nc <- numeric(), AIC.wc <- numeric(), BIC <- numeric())
  colnames(garch.ic) <- c("p", "q", "N", "LogLik", "AIC", "BIC")
  N <- length(residuals)
  
  for (q in 1:max.q) {
    for (p in 1:max.p) {
      garch <- garch(residuals, order=c(p,q), trace = F)
      aic <- -2*logLik(garch)[1]+2*(q+1)*(N/(N-q-2))
      bic <- AIC(garch, k=log(N)) # siehe help(BIC)
      garch.ic <- rbind(garch.ic, cbind(p, q, N, logLik(garch), aic, bic))
    }
  }
  
  aic.min <- garch.ic[garch.ic$aic == min(garch.ic$aic),]
  bic.min <- garch.ic[garch.ic$bic == min(garch.ic$bic),]
  
  print(garch.ic)
  print(aic.min)
  print(bic.min)
  # in der Funktion die lags auf Signifikanz testen?
}

# Rolling Forecast
fc.roll <- function(garchmodel) {
  startno <- 99
  SP.return <- na.omit(dailyReturn(data))
  startdate <- SP.return[1:startno]
  SPfc <- data.frame(Date = index(startdate), Return = as.numeric(startdate))
  forecast <- data.frame("Date", "Return")
  
  for (j in 1:5) {
    garch_fitted <- ugarchfit(garchmodel, SP.return[1:(startno+j)])
    garch.fc <- ugarchforecast(garch_fitted, SP.return[1:(startno+j)], n.ahead=1)
    SPfc <- rbind(SPfc, 
                  cbind.data.frame(
                    Date  = as.Date(index(SP.return[startno+j])),
                    Return = garch.fc@forecast$sigmaFor[1]))
    forecast <- as.xts(SPfc[,2], SPfc[,1])
  }
  forecast <- forecast[(startno+1):(startno+5)]
}


### Mein alter Teil
##_______________________________________________________
## Prepping the data
# UMSTIEG AUF DAILY RETURNS
# loading the data
data.raw <- read.csv2(file = url("https://raw.githubusercontent.com/SweetyHotGirl21/ECN430/master/SaP500.csv"), header = TRUE, stringsAsFactors = T)
data.crypto.raw <- read.csv2(file = url("https://raw.githubusercontent.com/SweetyHotGirl21/ECN430/master/BGCI.csv"), header = TRUE, stringsAsFactors = T)
data.crypto.raw[,3:4] <- NULL #delete the empty columns
#data.raw <- data.raw[1:9166,] #für einen kleineren Datensatz (bei Bedarf) #hier: nur vollständige Tage

data <- process(data.raw)
data.crypto <- process(data.crypto.raw)
chartSeries(data)
chartSeries(data.crypto)
#data <- data.crypto

# _______________________________________________________
## Calculate log returns and test for stationarity

# log returns
SP.return <- na.omit(diff(log(data)))
SP.daily <- na.omit(dailyReturn(data))
chartSeries(SP.return)
chartSeries(SP.daily)
View(SP.return)

plot(SP.return[which(as.Date(index(SP.return)) == "2019-10-18")]^2)

date <- "2019-10-18"
day <- SP.return[which(as.Date(index(SP.return)) == date)-2]
day <- day[-which(day== 0)]
plot(log(day)^2)
View(day)

adf.test(SP.return) #stationary (H0: unit root => non-stationary)
kpss.test(SP.return) #non-stationary (H0: x is level or trend stationary)

# ________________________________________________
## Step 1 (Box-Jenkins): ACF/PACF
ggAcf(SP.return)
ggPacf(SP.return)

# ________________________________________________
## Step 2: Model Estimation und Step 3: Diagnosis
arima <- auto.arima(SP.return, stepwise = F, approximation = F, trace=F)
lagorder <- c(arima$arma[1], arima$arma[2])
arima <- arima(SP.return, order = c(5,1,0))

# Test residuals
Box.test(resid(arima)) # H0 (H0: the data are independently distributed)
ArchTest(resid(arima)) #H1 (H0: no ARCH effects)

# Plot the squared residuals and evaluation with ACF/PACF
rsq <- resid(arima)^2
plot(rsq)
acf(rsq)
pacf(rsq)
checkresiduals(arima)

accuracy(arima) #für RMSE

# ________________________________________________
## Estimating an ARCH/GARCH model
garch.order(resid(arima), 20, 20) # => GARCH(1,1)
arch.order(resid(arima), 30) # => bic: GARCH(1,1), aic: ARCH(2) = GARCH(0,2)
# nur ein geringer unterschied bei den höheren Lags von ARCH(q) --> zur Vermeidung von overfitting ARCH(6)
# GARCH(1,1) zum Vergleich (schneidet fast so gut ab)

# test and compare a few select models
arch2.m <- garch(resid(arima), order=c(0,2), trace=F)
garch11.m <- garch(resid(arima), order=c(1,1), trace=F)

# residuals with the old model vs. new
plot(resid(arima))
plot(arch2.m$residuals)
plot(garch11.m$residuals)

# coefficients
arch2.m$coef #lags are all significant
garch11.m$coef #lags are all significant

# Realized volatility zum späteren Vergleich berechnen
realized.var <- xts(SP.return^2, index(SP.return))
rvar.daily <- aggregate(x = realized.var, FUN = sum, by = as.Date(index(realized.var)))
rvol.daily <- sqrt(rvar.daily)
rv.daily <- as.xts(data.frame(rvar.daily, rvol.daily))


garch <- ugarchspec(mean.model = list(armaOrder=c(5,1,0)))
garch

##_________________________________________________________________
# ARCH(0,2) für einen Tag im 5-Minuten-Takt forecasten --> reines ARCH geht gar nicht????
garch02 <- ugarchspec(variance.model=list(model="fGARCH",garchOrder=c(0,2),submodel="GARCH"), mean.model = list(armaOrder=lagorder))
garch_fitted02 <- ugarchfit(garch02, SP.return[1:(length(SP.return))])#-3)])
garch_fitted02@model$pars # Parameter (i.e. alpha, beta)
garch.fc02 <- ugarchforecast(garch_fitted02, SP.return, n.ahead=20)

#forecast sigma (rolling)
fc.garch02 <- fc.roll(garch02)


##________________________________________________________________
# GARCH(1,1) für einen Tag im 5-Minuten-Takt forecasten
# mit 5-min data
garch11 <- ugarchspec(variance.model=list(model="fGARCH",garchOrder=c(1,1),submodel="GARCH"), mean.model = list(armaOrder=lagorder))
garch_fitted11 <- ugarchfit(garch11, SP.return[1:(length(SP.return)-5)])#-35)])
garch_fitted11@model$pars # Parameter (i.e. alpha, beta)
garch.fc11 <- ugarchforecast(garch_fitted11, SP.return, n.ahead=1)


# mit Wechsel auf daily
garch_fitted11 <- ugarchfit(garch11, SP.daily[1:(length(SP.daily)-5)])
ugarchforecast(garch_fitted11)

sigma <- garch_fitted11@fit$sigma
sigma

var <- sigma^2
var

rvol.daily

plot(sigma, type="l")
fv <- garch_fitted11@fit$fitted.values #returns
plot(fv, type="l")


garch.fc11 <- ugarchforecast(garch_fitted11, SP.daily, n.ahead=5)

fc.garch11 <- fc.roll(garch11)


##________________________________________________________________
# Vergleich Forecasts
# Vergleich GARCH(1,1) forecast mit RV forecast
print(rv.daily[100:105])
garch.fc
garch.fc@forecast$sigmaFor[1]

# Vergleich ARCH(2) forecast mit RV forecast
print(rv.daily)
garch.fc
garch.fc@forecast$sigmaFor[1]


##_________________________________________________________________
# alle Plots

chartSeries(data)
chartSeries(SP.return)
ggAcf(SP.return)
ggPacf(SP.return)

plot(rsq)
acf(rsq)
pacf(rsq)

plot(rv.daily)

plot(resid(arima))
plot(arch2.m$residuals)
plot(garch11.m$residuals)

## Auswertungsplots
#Volatilität
plot(as.numeric(rv.daily[,2]), type="l")
lines(garch.fc11@model$modeldata$sigma, type="l", col="blue") #GARCH(1,1)
lines(garch.fc02@model$modeldata$sigma, type="l", col="green") #ARCH(2)

# Plot RV vs. Forecast of Sigma (using ugarchforecast())
rvd <- data.frame(index(rv.daily[100:104,2]), as.numeric(rv.daily[100:104,2]))
f11 <- data.frame(index(fc.garch11),as.numeric(fc.garch11)) #GARCH(1,1)
f02 <- data.frame(index(fc.garch02),as.numeric(fc.garch02)) #ARCH(2)

#plot of RV and Sigma Forecast
plot(rvd, main = "RV vs. forecast of Sigma", type="l")
lines(f11, col="blue")
lines(f02,col="green") #ARCH(2)
