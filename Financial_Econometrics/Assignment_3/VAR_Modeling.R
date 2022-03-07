# VAR Modeling

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

# Running a variable selection algorithm

generate_lags <- function(dataframe, max_lags) {

  for (name in names(dataframe)) {

    if(name %in% c("yyyymm", "Index")) next

    for (i in 1:max_lags) {

      var_names <- paste0(name, "_Lag", 1:max_lags)
      dataframe[var_names[i]] <- dplyr::lag(dataframe[name], n = i)

    }

  }

  return(dataframe %>% drop_na())

}

# Lagged Dataframe

# Define contemporaneous vector

leave_vars <- names(predictor_data_filtered)[!names(predictor_data_filtered) %in% c("log_returns", "yyyymm")]

lagged_dataframe <- generate_lags(predictor_data_filtered, max_lags=12) %>%
  dplyr::select(-all_of(leave_vars))

# Train / Test-Split (Test: Last 3 years, i.e. 2017-2019)
# It is a good practice to test feature importance only on the training data

train_data <- lagged_dataframe %>% dplyr::filter(yyyymm < "2017-01-01") %>% dplyr::select(-yyyymm)
test_data <- lagged_dataframe %>% dplyr::filter(yyyymm >= "2017-01-01") %>% dplyr::select(-yyyymm)

rf <- ranger(log_returns~., data=train_data, importance = "permutation")

imps <- importance(rf)

get_most_important_vars <- function(rf_model, variables_vector) {

  importances <- importance(rf_model)

  result_list <- list()

  init <- 1

  for (name in variables_vector) {

    named_importances <- names(importances)
    var_imp <- sum(unname(importances[grep(name, named_importances)]))
    result_list[init] <- var_imp
    init <- init + 1
  }

  return(result_list)
}

var_names <- names(predictor_data_filtered)[!names(predictor_data_filtered) %in% c("yyyymm", "Index")]

sort_by_most_important_var <- function(rf, var_names) {

  sum_imps <- get_most_important_vars(rf, var_names)
  names(sum_imps) <- var_names
  return(sum_imps[order(unlist(sum_imps),decreasing=TRUE)])
}

sort_by_most_important_var(rf, var_names)

# We now can retrieve the most important variables according to the Random Forest Estimator:
# b_m, tbl and ntis
# Interestingly, the lags of the log-returns is not among them.

# Before modeling, we will test all three selected variables for stationarity using the Augmented-Dickey-Fuller test

generate_adf_test_results <- function(data, max_lags) {

    res <- list()
    counter <- 1

    for (spec in c("nc", "c", "ct")) {

      test_result <- fUnitRoots::adfTest(data, lags=max_lags, type=spec)
      res[[counter]] <- test_result
      counter <- counter + 1

    }
    return(res)

}

res_bm <- generate_adf_test_results(predictor_data_filtered$b_m, max_lags=12)
# In none of the stationarity specifications b_m was stationary.

# We will difference b_m to make it stationary

bm_data <- predictor_data_filtered %>%
  mutate(b_m_stat = b_m - dplyr::lag(b_m))

res_bm_stat <- generate_adf_test_results(bm_data$b_m_stat, max_lags=12)
# In first differnces b_m is now stationary

res_log_returns <- generate_adf_test_results(predictor_data_filtered$log_returns, max_lags=12)
# The log returns are definitely stationary.

res_tbl <- generate_adf_test_results(predictor_data_filtered$tbl, max_lags=12)
# The Treasury bill rate is considered stationary in all three specifications.

# Generating Train/Test-Split for featured data
# Because we loose one observation due to differencing of b_m, we shorten the training set by one.
train_data_model <- predictor_data_filtered %>% dplyr::filter(yyyymm < "2016-12-01") %>% dplyr::select(-yyyymm)
test_data_model <- predictor_data_filtered %>% dplyr::filter(yyyymm >= "2016-12-01") %>% dplyr::select(-yyyymm)

# For cointegration tests to make sense, the variables would need to be all I(1), but they are not, so w
#jotest=ca.jo(dplyr::select(predictor_data_filtered, c(b_m, tbl, log_returns)) %>% drop_na(), type="trace", K=12, ecdet="none", spec="longrun")
#summary(jotest)

train_data_model_stat <- train_data_model %>%
  mutate(b_m_stat = b_m - dplyr::lag(b_m)) %>%
  drop_na()

test_data_model_stat <- test_data_model %>%
  mutate(b_m_stat = b_m - dplyr::lag(b_m)) %>%
  drop_na()

# Modeling the results

model_selector <- VARselect(dplyr::select(train_data_model_stat, c(b_m_stat, tbl, log_returns)) %>% drop_na(), lag.max=12)
var_model <- tsDyn::lineVar(dplyr::select(train_data_model_stat, c(b_m_stat, tbl, log_returns)) %>% drop_na(), lag=11, model="VAR")

# Generating predictions
predictions <- predict_rolling(var_model, nroll=nrow(test_data_model_stat), n.head=1, newdata = dplyr::select(test_data_model_stat, c(b_m_stat, tbl, log_returns)) %>% drop_na())

# To-Do: Generate RMSFE and MAFE for data

pred <- predictions$pred
true <- predictions$true

generate_metric_summary <- function(pred, true, variable_name, metric) {

  pred <- pred[variable_name]
  true <- true[variable_name]

  combined <- bind_cols(pred, true)
  colnames(combined) <- c("pred", "true")

  return(metric(combined, "true", "pred"))

}

rmse_var <- generate_metric_summary(pred, true, "log_returns", rmse)
mae_var <- generate_metric_summary(pred, true, "log_returns", mae)

print(rmse_var)
print(mae_var)

# Exporting loss series

pred_log_returns <- pred$log_returns
true_log_returns <- true$log_returns

loss <- true_log_returns - pred_log_returns

residuals_data_var <- dplyr::tibble(residuals = loss)

readr::write_csv(residuals_data_var, "residuals_data_var.csv")
