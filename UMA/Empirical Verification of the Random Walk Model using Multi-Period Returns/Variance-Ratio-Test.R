######
# Before this script can be run, the packages, variables and functions must be loaded from "BA_BCHerbert_MAIN" into the R session
######
### Variance ratio test

adf.test(Google.ts$Google_Return)
adf.test(GS.ts$GS_Return)
adf.test(PuG.ts$PuG_Return)
adf.test(NFLX.ts$NFLX_Return)

ArchTest(Google.ts$Google_Return, lags = 12)
ArchTest(GS.ts$GS_Return, lags = 12)
ArchTest(PuG.ts$PuG_Return, lags = 12)
ArchTest(NFLX.ts$NFLX_Return, lags = 12)

ArchTest(Google.ts$Google_Return, lags = 6)
ArchTest(GS.ts$GS_Return, lags = 6)
ArchTest(PuG.ts$PuG_Return, lags = 6)
ArchTest(NFLX.ts$NFLX_Return, lags = 6)

par(mfcol = c(3,2))
forecast::Acf(Google.ts$Google_Return, lag.max = 40, main = "ACF Alphabet")
forecast::Acf(PuG.ts$PuG_Return, lag.max = 40, main = "ACF Procter & Gamble")
forecast::Acf(NFLX.ts$NFLX_Return, lag.max = 40, main = "ACF Netflix")

forecast::Pacf(Google.ts$Google_Return, lag.max = 40, main = "PACF Alphabet", )
forecast::Pacf(PuG.ts$PuG_Return, lag.max = 40, main = "PACF Procter & Gamble")
forecast::Pacf(NFLX.ts$NFLX_Return, lag.max = 40, main = "PACF Netflix")
par(mfcol = c(1,1))
forecast::Pacf(GS.ts$GS_Return, lag.max = 40, main = "PACF Goldman Sachs")
forecast::Acf(GS.ts$GS_Return, lag.max = 40, main = "ACF Goldman Sachs")

### Fitting ARMA(p,q) Model
Google.ts.arima <- auto.arima(Google.ts$Google_Return, stationary = TRUE, ic = c("aic"), max.p = 10, max.P = 10)
Google.ts.arima
PuG.ts.arima <- auto.arima(PuG.ts$PuG_Return,  stationary = TRUE, ic = c("aic"), max.p = 10, max.P = 10)
PuG.ts.arima
NFLX.ts.arima.aic <- auto.arima(NFLX.ts$NFLX_Return,  stationary = TRUE, ic = c("aic"), max.p = 10, max.P = 10)
NFLX.ts.arima.aic
NFLX.ts.arima.bic <- auto.arima(NFLX.ts$NFLX_Return,  stationary = TRUE, ic = c("aic"), max.p = 10, max.P = 10)
NFLX.ts.arima.bic
NFLX.ts.arima <- NFLX.ts.arima.aic 

### GARCH Model

### Alphabet
Google.ts.arima$arma
###Normal distribution
Google.ts.garch11 <- garchFit(formula = ~garch(1,1), data = Google.ts.arima$residuals, trace = TRUE)
Google.ts.garch21 <- garchFit(formula = ~garch(2,1), data = Google.ts.arima$residuals, trace = TRUE)
Google.ts.garch12 <- garchFit(formula = ~garch(1,2), data = Google.ts.arima$residuals, trace = TRUE)
Google.ts.garch22 <- garchFit(formula = ~garch(2,2), data = Google.ts.arima$residuals, trace = TRUE)
###students t distribution
Google.ts.garch11.st <- garchFit(formula = ~garch(1,1), data = Google.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))
Google.ts.garch21.st <- garchFit(formula = ~garch(2,1), data = Google.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))
Google.ts.garch12.st <- garchFit(formula = ~garch(1,2), data = Google.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))
Google.ts.garch22.st <- garchFit(formula = ~garch(2,2), data = Google.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))

summary(Google.ts.garch11)
summary(Google.ts.garch21) 
summary(Google.ts.garch12)
summary(Google.ts.garch22)
summary(Google.ts.garch11.st)
summary(Google.ts.garch21.st) 
summary(Google.ts.garch12.st)
summary(Google.ts.garch22.st)

plot(Google.ts.garch@sigma.t, type = "l", main = "Conditional SD Alphabet")

### Procter and Gamble
PuG.ts.arima$arma
###Normal distribution
PuG.ts.garch11 <- garchFit(formula = ~garch(1,1), data = PuG.ts.arima$residuals, trace = TRUE)
PuG.ts.garch21 <- garchFit(formula = ~garch(2,1), data = PuG.ts.arima$residuals, trace = TRUE)
PuG.ts.garch12 <- garchFit(formula = ~garch(1,2), data = PuG.ts.arima$residuals, trace = TRUE)
PuG.ts.garch22 <- garchFit(formula = ~garch(2,2), data = PuG.ts.arima$residuals, trace = TRUE)
###students t distribution
PuG.ts.garch11.st <- garchFit(formula = ~garch(1,1), data = PuG.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))
PuG.ts.garch21.st <- garchFit(formula = ~garch(2,1), data = PuG.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))
PuG.ts.garch12.st <- garchFit(formula = ~garch(1,2), data = PuG.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))
PuG.ts.garch22.st <- garchFit(formula = ~garch(2,2), data = PuG.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))

summary(PuG.ts.garch11)
summary(PuG.ts.garch21) 
summary(PuG.ts.garch12)
summary(PuG.ts.garch22)
summary(PuG.ts.garch11.st)
summary(PuG.ts.garch21.st) 
summary(PuG.ts.garch12.st)
summary(PuG.ts.garch22.st)

### Netflix
NFLX.ts.arima$arma
###Normal distribution
NFLX.ts.garch11 <- garchFit(formula = ~garch(1,1), data = NFLX.ts.arima$residuals, trace = TRUE)
NFLX.ts.garch21 <- garchFit(formula = ~garch(2,1), data = NFLX.ts.arima$residuals, trace = TRUE)
NFLX.ts.garch12 <- garchFit(formula = ~garch(1,2), data = NFLX.ts.arima$residuals, trace = TRUE)
NFLX.ts.garch22 <- garchFit(formula = ~garch(2,2), data = NFLX.ts.arima$residuals, trace = TRUE)
###students t distribution
NFLX.ts.garch11.st <- garchFit(formula = ~garch(1,1), data = NFLX.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))
NFLX.ts.garch21.st <- garchFit(formula = ~garch(2,1), data = NFLX.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))
NFLX.ts.garch12.st <- garchFit(formula = ~garch(1,2), data = NFLX.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))
NFLX.ts.garch22.st <- garchFit(formula = ~garch(2,2), data = NFLX.ts.arima$residuals, trace = TRUE, cond.dist = c("std"))

summary(NFLX.ts.garch11)
summary(NFLX.ts.garch21) 
summary(NFLX.ts.garch12)
summary(NFLX.ts.garch22)
summary(NFLX.ts.garch11.st)
summary(NFLX.ts.garch21.st) 
summary(NFLX.ts.garch12.st)
summary(NFLX.ts.garch22.st)

par(mfcol = c(1,3))
plot(Google.ts.garch11.st@sigma.t, type = "l", main = "Conditional SD Alphabet", xlab = "5-minutes interval", ylab = "")
plot(PuG.ts.garch21.st@sigma.t, type = "l", main = "Conditional SD P&G", xlab = "5-minutes interval", ylab = "")
plot(NFLX.ts.garch21.st@sigma.t, type = "l", main = "Conditional SD Netflix", xlab = "5-minutes interval", ylab = "")

par(mfcol = c(1,3))
plot(Google.ts.garch11.st@sigma.t^2, type = "l", main = "Conditional Variance Alphabet", xlab = "5-minutes interval", ylab = "")
plot(PuG.ts.garch21.st@sigma.t^2, type = "l", main = "Conditional Variance P&G", xlab = "5-minutes interval", ylab = "")
plot(NFLX.ts.garch21.st@sigma.t^2, type = "l", main = "Conditional Variance Netflix", xlab = "5-minutes interval", ylab = "")


Lo.Mac(Google.ts$Google_Return, k = c(6, 12, 24, 80, 160))
Lo.Mac(GS.ts$GS_Return, k = c(6, 12, 24, 80, 160))
Lo.Mac(PuG.ts$PuG_Return, k = c(6, 12, 24, 80, 160))
Lo.Mac(NFLX.ts$NFLX_Return, k = c(6, 12, 24, 80, 160))
