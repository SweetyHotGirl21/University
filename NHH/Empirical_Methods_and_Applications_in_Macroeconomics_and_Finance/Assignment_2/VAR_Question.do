*Assignment 2 - VAR model
//log using varquestion_log, replace

*Preparing data
clear
clear matrix
import excel quarterly.xls, clear firstrow
gen t = _n
gen time = tq(1960q1) + t - 1
format time %tq
tsset t

*Generating variables
gen intspread = r10-Tbill
gen lnindpro = ln(indpro)
gen lnindpro_change = lnindpro  - l1.lnindpro
gen s_ur = urate - l4.urate

keep Date intspread lnindpro_change s_ur lnindpro r10 Tbill urate t 

summarize intspread lnindpro_change s_ur

dfuller intspread, trend regress
dfuller lnindpro_change, trend regress
dfuller s_ur, trend regress
kpss intspread
kpss lnindpro_change
kpss s_ur

tsline intspread, name(intspread, replace) title("Interest rate spread") ytitle("in %")
tsline lnindpro_change, name(lnindpro_change, replace) title("Log  change industrial production") ytitle("in %")
tsline s_ur, name(s_ur, replace) title("Seasonal difference unemploymant rate") ytitle("in %")

graph intspread lnindpro_change s_ur

*varsoc lnindpro_change s_ur intspread, maxlag(8)


*Estimating three variable VAR model, 8 lags and AIC BIC
var lnindpro_change s_ur intspread, dfk lags(1/8) 
varsoc lnindpro_change s_ur intspread, maxlag(8) lutstats
*varwle
varstable, graph
predict var1res, res //replace
wntestq var1res
summarize var1res
tsline var1res, name(var1res, replace) title("Residuals VAR model with 8 Lags")
est store var1
esttab var1 using var1.tex, replace
estat ic

*varsoc if time>=12, maxlag(8) lutstats

*Estimating three variable model, now 3 lags, AIC and BIC 
var d.lnindpro s_ur intspread if t > 12, lutstats dfk lags(1/3)
varstable, graph
predict var2res, res //yreplace
wntestq var2res
tsline var1res, name(var2res, replace) title("Residuals VAR model with 3 Lags")
est store var2
estat ic



*Testing
var d.lnindpro s_ur intspread, lutstats lags(1 2 3 4 5 6 7 8)
est store varA
var d.lnindpro s_ur intspread if t > 12, lutstats lags(1 2 3)
est store varB

lrtest varB varA, df(45)

irf create order1, step(8) set(myirf1)
irf graph oirf, impulse(D.lnindpro) response(s_ur)
graph export dindpro_dur.eps, replace
irf graph oirf, impulse(D.lnindpro) response(intspread)
graph export dindpro_intspready.eps, replace

irf table oirf fevd, irf(order1) noci std




var d.lnindpro s_ur intspread, lutstats lags(1/3) 
vargranger

irf create order1, step(8) set(myirf1)
irf graph oirf, impulse(D.lnindpro) response(s_ur)
irf table oirf fevd, impulse(D.lnindpro) response(s_ur intspread) step(1)

graph export dindpro_dur.eps, replace
irf graph oirf, impulse(D.lnindpro) response(intspread)
graph export dindpro_intspready.eps, replace

irf table oirf fevd, step(1)
irf table oirf fevd, noci std step(1)
