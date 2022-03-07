rm(list = ls())

# Setting Seed
set.seed(42)

# Importing libraries
library(tsDyn)
library(forecast)
library(tidyverse)
library(readxl)
library(fs)
library(lubridate)
library(vars)
library(yardstick)
library(ranger)
library(dplyr)
library(fUnitRoots)
library(naniar)
library(pracma)
library()

library(rugarch)
library(xts)

# Reading in the data and preparing for modeling

# A simple utility function

coerce_to_double <- function(col) {
  if (is.character(col)) {
    return(as.double(col))
  } else {
    return(col)
  }
}


predictor_data <- read_excel("Desktop/The_b_biz/Financial Econometrics/Assignment 3/PredictorData2019.xlsx", sheet = "Monthly")

# By inspection we see that "NA" values are encoded as "NaN".

predictor_data_na_repl <- predictor_data %>% replace_with_na_all(condition = ~.x == "NaN")

predictor_data_filtered_intm <- predictor_data_na_repl %>%
  mutate(yyyymm = ymd(paste0(yyyymm,"01"))) %>%
  dplyr::filter(yyyymm >= "2000-01-01") %>%
  mutate(log_returns = log(Index) - dplyr::lag(log(Index))) %>%
  rename(b_m=`b/m`)  %>%
  mutate_each(coerce_to_double)

miss_var_summary(predictor_data_filtered_intm)

# We drop the "csp" variable as 85% of the data is missing there

predictor_data_filtered_intm$csp <- NULL
predictor_data_filtered <- predictor_data_filtered_intm %>% dplyr::filter(row(predictor_data_filtered_intm)>1)



predictor_data_filtered$yyyymm = as.Date(predictor_data_filtered$yyyymm, format = "%Y-%m-%d")
data_ts = xts(predictor_data_filtered[,2:18], order.by = predictor_data_filtered$yyyymm)

data_train = data_ts["/2016"]
data_test = data_ts["2017/"]

spec.11.norm = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                                        mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                                        distribution.model = "norm")
fit.11.norm = ugarchfit(spec.11.norm, data_ts$log_returns)
plot(fit.11.norm)
12
0
spec.GJR.norm = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), 
                     mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                     distribution.model = "norm")
fit.GJR.norm = ugarchfit(spec.GJR.norm, data_ts$log_returns)
plot(fit.GJR.norm)
12
0
spec.11.std = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                     mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                     distribution.model = "std")
spec.GJR.std = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), 
                      mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                      distribution.model = "std")

roll.11.norm = ugarchroll(spec.11.norm, data_ts$log_returns, n.start = 203, refit.every = 1,
                          refit.window = "moving", solver = "hybrid", keep.coef = TRUE, n.ahead = 1) #, calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05), cluster = cl, keep.coef = TRUE)
fpm(roll.11.norm)
plot(roll.11.norm, which = 5)
plot(roll.11.norm)



2
roll.GJR.norm = ugarchroll(spec.GJR.norm, data_ts$log_returns, n.start = 203, refit.every = 1,
                           refit.window = "moving", solver = "hybrid", n.ahead = 1)
fpm(roll.GJR.norm)
plot(roll.GJR.norm, which = 5)

roll.11.std = ugarchroll(spec.11.std, predictor_data_filtered$log_returns, n.start = 203, refit.every = 1,
                          refit.window = "moving", solver = "hybrid", n.ahead = 1)
fpm(roll.11.std)
plot(roll.11.std, which = 5)

roll.GJR.std = ugarchroll(spec.GJR.std, predictor_data_filtered$log_returns, n.start = 203, refit.every = 1,
                          refit.window = "moving", solver = "hybrid", n.ahead = 1)

fpm(roll.GJR.std)
plot(roll.GJR.std, which = 5)


spec.11.norm.ex = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), external.regressors = data_ts$b_m), 
                         mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                         distribution.model = "norm")
spec.GJR.norm.ex = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), external.regressors = data_ts$b_m), 
                           mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                           distribution.model = "norm")
roll.11.norm.ex = ugarchroll(spec.11.norm.ex, data_ts$log_returns, n.start = 203, refit.every = 1,
                          refit.window = "moving", solver = "hybrid", keep.coef = TRUE, n.ahead = 1) 
roll.GJR.norm.ex = ugarchroll(spec.11.std.ex, data_ts$log_returns, n.start = 203, refit.every = 1,
                            refit.window = "moving", solver = "hybrid", keep.coef = TRUE, n.ahead = 1) 
plot(roll.11.norm.ex, which = 5)
plot(roll.GJR.norm.ex, which = 5)
fpm(roll.11.norm.ex)
fpm(roll.GJR.norm.ex)

### GARCH(1,1) with standard normal error terms is the best model, smallest MSE
roll.11.norm@forecast$density$Mu
data_test$log_returns

show(roll.11.norm)
plot(roll.11.norm)
roll.11.norm@forecast$density$Realized
error = roll.11.norm@forecast$density$Realized - roll.11.norm@forecast$density$Mu


squred.returns = as.data.frame(roll.11.norm@forecast$density$Realized)

library("MCS")
LossVol()
