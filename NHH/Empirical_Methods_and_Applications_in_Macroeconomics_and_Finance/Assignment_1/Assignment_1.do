clear
clear matrix

import excel "C:\Users\B-C-Herbert\Documents\Studium\Mannheim\VWL\2020 - 2020 SS\Empirical Methods and Applications in Macroeconomics and Finance\Assignment\Assignment1\Data.xlsx", firstrow clear

scalar Prep = 1 /*Prepearing Data for time series and naming relevant Data*/
scalar taskA = 1
scalar taskB = 1
scalar taskC = 1
scalar taskD = 1
scalar taskE = 1
scalar taskF_G = 1
/*Forecasting*/
scalar taskH = 1
scalar taskI = 1
scalar taskJ = 1

if scalar(Prep) == 1 {
gen quarter= qofd(A)
format quarter %tq
tsset quarter 
drop A
/* Rename Variable GDP */
gen GDP = GDP_Norway_seas_adj
drop GDP_Norway_seas_adj
label variable GDP "Gross Domestic Product Norway Mainland"
gen GDP_se = GDP_Norway_not_adj
drop GDP_Norway_not_adj
label variable GDP_se "Gross Domestic Product Norway Mainland (seasonal)"
/*Calculate log(GDP)*/
gen logGDP = log(GDP)
label variable logGDP "Log Gross Domestic Product Norway Mainland"
/*First difference of LogGDP*/
gen fdlogGDP = S.logGDP*100
label variable fdlogGDP "First Log difference Gross Domestic Product Norway Mainland"
/*Year to Year growth of GDP*/
gen growth = S4.logGDP*100
label variable growth "Year to Year Growth rate Gross domestic product Mainland"
}

if scalar(taskA) == 1 {
tsline GDP, name(GDP, replace) title("GDP Norway Mainland") ytitle("NOK in Millio")
tsline logGDP, name(logGDP, replace) title("Log GDP Norway Mainland") ytitle("Log NOK in Million")
tsline fdlogGDP, name(fdlogGDP, replace) title("First Difference Log GDP Norway Mainland") ytitle("in %")
tsline growth, name(growth, replace) title("Year to Year Growth Rate GDP Norway Mainland") ytitle("in %")

graph combine GDP logGDP fdlogGDP growth
}

if scalar(taskB) == 1 {
/*Correlgogram */
/* Log(GDP)*/
corrgram logGDP, lags(30) yw
ac logGDP, lags(30) name(AClevellogGDP, replace) nodraw title("Autocorrelations, log(GDP)") ytitle("")
pac logGDP, lags(30) name(PAClevellogGDP, replace) yw nodraw title("Partical ac, log(GDP)") ytitle("")
graph combine AClevellogGDP PAClevellogGDP, name(LevelGph, replace) 
graph display LevelGph, ysize(6) xsize(10) scale(1.4)


/* First difference log(GDP)*/
corrgram fdlogGDP, lags(30) yw
ac fdlogGDP, lags(30) name(AClevelfdGDP, replace) nodraw title("Autocorrelations, first dif. log(GDP)") ytitle("")
pac fdlogGDP, lags(30) name(PAClevelfdGDP, replace) yw nodraw title("Partical ac, first dif. log(GDP)") ytitle("")
graph combine AClevelfdGDP PAClevelfdGDP , name(LevelGphfdGDP, replace) 
graph display LevelGphfdGDP, ysize(6) xsize(10) scale(1.4)

/* YtY Growth*/
corrgram growth, lags(30) yw
ac growth, lags(30) name(AClevelgrowth, replace) nodraw title("Autocorrelations, GDP Growth") ytitle("")
pac growth, lags(30) name(PAClevelgrowth, replace) yw nodraw title("Partical ac, GDP Growth") ytitle("")
graph combine AClevelgrowth PAClevelgrowth , name(Levelgrowth, replace) 
graph display Levelgrowth, ysize(6) xsize(10) scale(1.4)

}

if scalar(taskC) == 1 {
/*Calculate log(GDP_se)*/
gen logGDP_se = log(GDP_se)
label variable logGDP_se "Log Gross Domestic Product Norway Mainland (seasonal)"
/*First difference of LogGDP_se*/
gen fdlogGDP_se = (logGDP_se - L1.logGDP_se)*100
label variable fdlogGDP_se "First Log difference Gross Domestic Product Norway Mainland (seasonal)"
/*Year to Year growth of GDP_se*/
gen growth_se = (logGDP_se - L4.logGDP_se)*100
label variable growth_se "Year to Year Growth rate Gross domestic product Mainland (seasonal)"

/* quater dummies for regression */
egen dum1 = fill(1 0 0 0 1 0 0 0)
egen dum2 = fill(0 1 0 0 0 1 0 0)
egen dum3 = fill(0 0 1 0 0 0 1 0)
egen dum4 = fill(0 0 0 1 0 0 0 1)
gen trend = _n

/* Regression with seasonal unadjusted data on quarter dummies */
eststo: reg logGDP_se trend dum1 dum2 dum3
eststo: newey logGDP_se trend dum1 dum2 dum3, lag(4)
esttab, p title(OLS and Newey-West Regression on logGDP without seasonal adjustment) mtitles("reg logGDP" "newey logGDP")
eststo clear

eststo: reg fdlogGDP_se trend dum1 dum2 dum3
eststo: reg fdlogGDP_se dum1 dum2 dum3
eststo: newey fdlogGDP_se trend dum1 dum2 dum3, lag(4)
eststo: newey fdlogGDP_se dum1 dum2 dum3, lag(4)
esttab, p title(OLS and Newey-West Regression on First Difference LogGDP without seasonal adjustment) mtitles("reg fdlogGDP" "newey fdlogGDP" "reg fdlogGDP" "newey fdlogGDP" )
eststo clear

eststo: reg growth_se trend dum1 dum2 dum3
eststo: reg growth_se dum1 dum2 dum3
eststo: newey growth_se trend dum1 dum2 dum3, lag(4)
eststo: newey growth_se dum1 dum2 dum3, lag(4)
esttab, p title(OLS and Newey-West Regression on Year-to-Year Growth without seasonal adjustment) mtitles("reg growth" "newey growth" "reg growth" "newey growth" )
eststo clear

/* Regression with seasonal adjusted data on quarter dummies */
eststo: reg logGDP trend dum1 dum2 dum3
eststo: newey logGDP trend dum1 dum2 dum3, lag(4)
esttab, p title(OLS and Newey-West Regression on logGDP with  seasonal adjustment)  mtitles("reg logGDP" "newey logGDP")
eststo clear

eststo: reg fdlogGDP trend dum1 dum2 dum3
eststo: reg fdlogGDP dum1 dum2 dum3
eststo: newey fdlogGDP trend dum1 dum2 dum3, lag(4)
eststo: newey fdlogGDP dum1 dum2 dum3, lag(4)
esttab, p title(OLS and Newey-West Regression on First Difference LogGDP with seasonal adjustment) mtitles("reg fdlogGDP" "newey fdlogGDP" "reg fdlogGDP" "newey fdlogGDP" )
eststo clear

eststo: reg growth trend dum1 dum2 dum3
eststo: reg growth dum1 dum2 dum3
eststo: newey growth trend dum1 dum2 dum3, lag(4)
eststo: newey growth dum1 dum2 dum3, lag(4)
esttab, p title(OLS and Newey-West Regression on Year-to-Year Growth with seasonal adjustment)  mtitles("reg growth" "newey growth" "reg growth" "newey growth" )
eststo clear

/* seasonal adjustment*/
/* GDP */
tssmooth ma GDPSAma = GDP_se, window(2 1 2)
tsline GDP_se GDPSAma , name(GDPSAma, replace) legend(label( 1 "Not SA") label(2 "SA with MA")) title("GDP Norway Mainland")
graph display GDPSAma, ysize(6) xsize(10) scale(1.4)

eststo: reg GDPSAma trend dum1 dum2 dum3

/* logGDP */
tssmooth ma logGDPSAma = logGDP_se, window(2 1 2)
tsline logGDP_se logGDPSAma, name(logGDPSAma, replace) legend(label( 1 "Not SA") label(2 "SA with MA")) title("logGDP Norway Mainland")
graph display logGDPSAma, ysize(6) xsize(10) scale(1.4)

eststo: reg logGDPSAma trend dum1 dum2 dum3

/* First difference Log GDP*/
tssmooth ma fdlogGDPSAma = fdlogGDP_se, window(1 1 1)
tsline fdlogGDP_se fdlogGDPSAma, name(fdlogGDPSAma, replace) legend(label( 1 "Not SA") label(2 "SA with MA")) title("first dif. logGDP Norway Mainland")
graph display fdlogGDPSAma, ysize(6) xsize(10) scale(1.4)

eststo: reg fdlogGDPSAma dum1 dum2 dum3
esttab, p title(OLS Regression on seasonal adjusted data)
eststo clear
}

if scalar(taskD) == 1 {

corrgram logGDP, lags(8) yw
varsoc logGDP, maxlag(8) /*non stationary, moving avarage MA(.) ARMA(1 5)*/
dfuller logGDP, regress lags(5) trend  /*lags based on AIC */
/* H0 can't be rejected: so there is no stationary*/
kpss logGDP /*H0 cat'be rejected: trend stationary*/


corrgram fdlogGDP, lags(8) yw
varsoc fdlogGDP, maxlag(8) /*malag() based on AIC */
dfuller fdlogGDP, regress lags(3)
kpss fdlogGDP, notrend /* is stationary */


corrgram growth, lags(8) yw
varsoc growth, maxlag(8)
dfuller growth, regress lags(4)
kpss growth, notrend

}

if scalar(taskE) == 1 {

corrgram growth, lags(20) yw
varsoc growth, maxlag(8)
ac growth, lags(30) name(AClevelgrowth, replace) nodraw title("Autocorrelations, GDP Growth") ytitle("")
pac growth, lags(30) name(PAClevelgrowth, replace) yw nodraw title("Partical ac, GDP Growth") ytitle("")
graph combine AClevelgrowth PAClevelgrowth , name(Levelgrowth, replace) 
graph display Levelgrowth, ysize(6) xsize(10) scale(1.4)
}

if scalar(taskF_G) == 1{
/*An common approch is to itereate through diffrent model specification looking for an minimized Information Criteria*/

/*Searcing for minimized BIC value */

local m_ar = 0
local m_ma = 0
local m_bic = 99999
forvalue v = 0(1)8{
forvalue w = 0(1)8{
dis "ar = " `v' ", ma = " `w'
qui arima growth, arima(`v',0,`w')
qui estat ic
mat temp = r(S)
local bic = temp[1,6]
if `bic' < `m_bic' {
local m_bic = `bic'
local m_ar = `v'
local m_ma = `w'
}
}
}
display "Estimated model by minimizied BIC: ARIMA(" `m_ar' " 0 " `m_ma' " )"
arima growth, arima(`m_ar',0,`m_ma')

/*Searcing for minimized AIC value*/

local m_ar = 0
local m_ma = 0
local m_aic = 99999
forvalue v = 0(1)8{
forvalue w = 0(1)8{
dis "ar = " `v' ", ma = " `w'
qui arima growth, arima(`v',0,`w')
qui estat ic
mat temp = r(S)
local aic = temp[1,6]
if `aic' < `m_aic' {
local m_aic = `aic'
local m_ar = `v'
local m_ma = `w'
}
}
}
display "Estimated model by minimizied AIC: ARIMA(" `m_ar' " 0 " `m_ma' " )"
arima growth, arima(`m_ar',0,`m_ma')
*/

/*Estiamted model from iteration approch: ARMA(1 5) */
arima growth, arima(1, 0, 5)

/*Since there are no significant Lag in the MA part of the model, respecification has to be done */


arima growth, ar(1) ma(1 2 5)
/*All lags in this model are significant*/

/* Predicting Fitted Values and Residuals */
predict fitted_growth_arma, xb 
predict res_growth_arma, residuals 
tsline growth fitted_growth_arma, name(Fitted_arma, replace) title("Fittet values GDP growth Norway") legend(label(1 "Data") label(2 "Fitted_values"))
tsline res_growth_arma, name(Residuals_arma_line, replace) title("Residuals GDP growth Norway") legend(label(1 "Data") label(2 "Residuals"))
histogram res_growth_arma, frequency normal bin(20) name(Residuals_arma_histogramm, replace) title("Residuals GDP growth Norway") legend(label(1 "Data") label(2 "Residuals"))
 
corrgram res_growth_arma, lags(30) yw
ac res_growth_arma, lags(30) name(AC_Res_Growth_ARMA, replace) nodraw title("Autocorrelations, Residuals ARMA(1 5)") ytitle("")
pac res_growth_arma, lags(30) name(PAC_Res_Growth_ARMA, replace) yw nodraw title("Partical ac, Residuals ARMA(1 5)") ytitle("")
graph combine Residuals_arma_line Residuals_arma_histogramm AC_Res_Growth_ARMA PAC_Res_Growth_ARMA, name(LevelGph, replace) 

/* Eigenvalues and Unit root*/
estat aroots
}

if scalar(taskH) == 1 {
arima growth, arima(2,0,2)
estat ic
predict fitted_growth_22, xb 
predict res_growth_22, residuals

arima growth, ar(1) ma(1 2 5)
estat ic

tsline growth fitted_growth_22 fitted_growth_arma, name(Fitted_arma, replace) title("Fittet values GDP growth Norway") legend(label(1 "GDP Growth") label(2 "Fitted Values ARMA(2 2)") label(3 "Fitted Values ARMA(1 5)"))
}

if scalar(taskI) == 1 {
/*Reestimation of both models until T=2012Q4 and forecast till the end of the sample*/
gen pred_date = tq(2012q4)
arima growth if(quarter<=pred_date), arima(2,0,2)

predict arma_22_pred, dynamic(pred_date)
predict arma_22_mse, mse dynamic(pred_date)
predict arma_22_res, residuals
gen ci_upper_22 = arma_22_pred + 1.96*sqrt(arma_22_mse) 
gen ci_lower_22 = arma_22_pred - 1.96*sqrt(arma_22_mse) 
tsline growth || tsline arma_22_pred ci_lower_22 ci_upper_22 if(quarter>=pred_date), tline(2012q4) name(Forecast_22, replace) title("Forecast from ARMA(2 2)model") ytitle("") nodraw legend(label(1 "Y-t-Y GDP Growth") label(2 "Prediction") label(3 "CI Lower") label(4 "CI Upper"))

arima growth if(quarter<=pred_date), ar(1) ma(1 2 5)
predict arma_15_pred, dynamic(pred_date)
predict arma_15_mse, mse dynamic(pred_date)
gen ci_upper_15 = arma_15_pred + 1.96*sqrt(arma_15_mse) 
gen ci_lower_15 = arma_15_pred - 1.96*sqrt(arma_15_mse) 
tsline growth || tsline arma_15_pred ci_lower_15 ci_upper_15 if(quarter>=pred_date), tline(2012q4) name(Forecast_15, replace) title("Forecast from ARMA(1 5)model") ytitle("") nodraw legend(label(1 "Y-t-Y GDP Growth") label(2 "Prediction") label(3 "CI Lower") label(4 "CI Upper"))
graph combine Forecast_15 Forecast_22, ysize(6) xsize(10) scale(1.4) 
}

if scalar(taskJ) == 1 {
gen arma_22_rmse = sqrt(arma_22_mse) 
gen arma_15_rmse = sqrt(arma_15_mse)
/* RMSE of ARMA(2 2) and ARMA(1 5)*/
display arma_22_rmse "   " arma_15_rmse


/*predict res_growth_22, resid
predict res_growth_arma, resid
*/
mean arma_22_res if(quarter>pred_date)
mean res_growth_arma if(quarter>pred_date)
/*These results suggest that our choosen model is still to prefer */


/* scc install hprescott */
/* The state of the Norwegian business cycle */
hprescott logGDP, smooth(1600) stub(hp)
gen LogGDPBCycle = hp_logGDP_1
gen LogGDPBTrend = hp_logGDP_sm_1 

tsline logGDP LogGDPBTrend, legend(label(1 "log(GDP)") label(2 "HP trend"))
tsline LogGDPBCycle, ytitle("Cycle (as a difference from trend)") tlabel(1980q1(80)2020q1)
}
