*Assignment 2 - Inflation and Stock Returns (Simultaneous equations)

cd "C:\Users\B-C-Herbert\Documents\Studium\Mannheim\VWL\2020 - 2020 SS\Empirical Methods and Applications in Macroeconomics and Finance\Assignment2"

*log using 2slsquestion_log, replace

*Preparing data

clear

use macro.dta
*gen t = _n, replace
gen time = tm(1986m3) + _n - 1
format time %tm
tsset 

*generate macro series (see assignment text)
replace inflation = 100*ln(CPI/l1.CPI)
gen returns = 100*ln(SANDP/l1.SANDP)
replace dcredit = CONSUMERCREDIT - l1.CONSUMERCREDIT
replace dprod = Industrialproduction - l1.Industrialproduction
replace dmoney = M1MONEYSUPPLY - l1.M1MONEYSUPPLY
replace dspread = BAAAAASPREAD - l1.BAAAAASPREAD
replace term = USTB10Y - 1l.USTB10Y
replace rterm = term - l1.term

sum, det

*2SLS using IV command
ivregress 2sls inflation dprod dcredit dmoney (returns = dcredit dprod rterm dspread dmoney)

*alternatively 2SLS "by hand"
regress returns dcredit dprod rterm dspread dmoney
predict returns_hat
predict u_returns_hat, resid
regress inflation dprod dcredit dmoney returns_hat

*test for endgeneity
regress inflation dprod dcredit dmoney returns u_returns_hat


ivregress 2sls returns dprod dspread rterm (inflation = dcredit dprod rterm dspread dmoney)


reg3 (inflation returns dprod dcredit dmoney) (returns inflation dprod dspread rterm)


