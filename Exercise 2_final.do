/******************************************************************************************
 APPENDIX B — STATA DO-FILE (Analysis of Executive Compensation Data)
 Purpose : Estimate link between firm size, firm profitability, shareholder value and CEO compensation
 Author  : Yannik Biebert, Jakob Gruss, Cayetano Llaca Kuri, Julian Vincent Müller, Victoria Walf
 Course  : Advanced Corporate Finance
******************************************************************************************/

*****************************************************
* 0) WORKING DIRECTORY
*****************************************************
* >>> UPDATE THIS PATH TO THE FOLDER CONTAINING THE .dta FILE <<< 
* Please replace the path below on your computer to the "execomp.dta" file

clear all
set more off
use "C:\Your\Path\Here", clear
cap which estpost
if _rc ssc install estout, replace

***********************************************
**Build variables**

keep if tdc1>0 & at>0
drop if missing(gvkey, tdc1, at, ni)
gen profitability    = ni/at
gen size             = ln(at)
gen log_compensation = ln(tdc1)
gen double cashpay   = cond(missing(salary),0,salary) + cond(missing(bonus),0,bonus) + cond(missing(othcomp),0,othcomp)
gen double log_cash  = ln(max(cashpay,1e-6))
gen double log_ltip  = ln(max(ltip,1e-6))
gen double ln_mkvalt = ln(mkvalt)

***************************************************
**Descriptives (winsorized 1%/99%)**

capture program drop _w
program define _w
    syntax varname, GEN(name)
    quietly _pctile `varlist', p(1 99)
    gen double `gen' = min(max(`varlist', r(r1)), r(r2))
end

local dvars salary bonus othcomp ltip tdc1 cashpay at ni sale mkvalt ///
            profitability size log_compensation log_cash log_ltip ln_mkvalt
local dvars_w
foreach v of local dvars {
    cap drop `v'_w
    _w `v', gen(`v'_w)
    local dvars_w `dvars_w' `v'_w
}

estpost tabstat `dvars_w', statistics(mean p50 sd min max n) columns(statistics)
esttab, cells("mean(fmt(3)) p50(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3)) n") ///
    nomtitle nonumber noobs label

*******************************************************
**2a)
**OLS: log pay on drivers**

capture program drop _w
program define _w
    syntax varname, GEN(name)
    quietly _pctile `varlist', p(1 99)
    gen double `gen' = min(max(`varlist', r(r1)), r(r2))
end
*build variables needed
capture gen profitability    = ni/at
capture gen size             = ln(at)
capture gen log_compensation = ln(tdc1)

*OLS raw + winsor (clustered by firm)
estimates clear
regress log_compensation profitability size, vce(cluster gvkey)
estimates store A1

foreach v in profitability size log_compensation {
    cap drop `v'_w
    _w `v', gen(`v'_w)
}
regress log_compensation_w profitability_w size_w, vce(cluster gvkey)
estimates store A2

esttab A1 A2, mtitles("2a OLS" "2a OLS (wins 1/99)") ///
  b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
  stats(N r2, labels("Obs." "R-sq."))
  
  
****************************************************
**2b)**
**Pay–Value sensitivity in aligned USD**

cap gen double mv_us  = mkvalt*1e6   // $Mn → USD
cap gen double pay_us = tdc1*1000    // $000 → USD
drop if missing(mv_us, pay_us)

estimates clear
regress pay_us mv_us, vce(cluster gvkey)
estimates store B1

cap drop pay_us_w mv_us_w
_w pay_us, gen(pay_us_w)
_w mv_us,  gen(mv_us_w)
regress pay_us_w mv_us_w, vce(cluster gvkey)
estimates store B2

*Convenient interpretation (per $1k and per $1m)
scalar b1_1k = _b[mv_us]*1000
scalar b2_1k = _b[mv_us_w]*1000
display as txt "2b Baseline:  $ per $1k MV = " as res %9.4f b1_1k
display as txt "2b Wins 1/99: $ per $1k MV = " as res %9.4f b2_1k

esttab B1 B2, mtitles("2b USD levels" "2b USD levels (wins 1/99)") ///
  b(%9.6f) se(%9.6f) star(* 0.10 ** 0.05 *** 0.01) ///
  stats(N r2, labels("Obs." "R-sq."))
  
********************************************************
**2c)**
**Gender heterogeneity**

capture drop female
gen byte female = inlist(lower(strtrim(gender)),"female","f","woman","w")
drop if missing(female)

*baseline (USD levels)
regress pay_us c.mv_us##i.female, vce(cluster gvkey)
lincom mv_us
scalar male_1k  = r(estimate)*1000
lincom mv_us + 1.female#c.mv_us
scalar fem_1k   = r(estimate)*1000
test 1.female#c.mv_us
scalar p_diff   = r(p)

*winsorized (1/99)
regress pay_us_w c.mv_us_w##i.female, vce(cluster gvkey)
lincom mv_us_w
scalar male_1k_w = r(estimate)*1000
lincom mv_us_w + 1.female#c.mv_us_w
scalar fem_1k_w  = r(estimate)*1000
test 1.female#c.mv_us_w
scalar p_diff_w  = r(p)

*Summary table
matrix H = (male_1k, male_1k_w \ fem_1k, fem_1k_w \ p_diff, p_diff_w)
matrix rownames H = "Male: $ per $1k ΔMV" "Female: $ per $1k ΔMV" "p: slope diff."
matrix colnames H = "Baseline" "Wins 1/99"
matlist H, format(%9.4f) names

*************************************************
**2d)**
**Panel FE with year effects**

* one observation per firm–year, this to avoid duplicates
bys gvkey year: keep if _n==1
xtset gvkey year

*(a) FE for ln pay ~ profitability + size + year FE
estimates clear
xtreg log_compensation profitability size i.year, fe vce(cluster gvkey)
estimates store D1

*(b) FE for pay ~ MV + year FE  (tdc1=$000, mkvalt=$Mn)
xtreg tdc1 mkvalt i.year, fe vce(cluster gvkey)
estimates store D2

esttab D1, mtitles("2d(a) FE: lnPay ~ Prof + Size + Year") ///
  b(%9.4f) se(%9.4f) stats(N r2_within, labels("Obs." "R2 within"))
esttab D2, mtitles("2d(b) FE: Pay ~ MV + Year") ///
  b(%9.6f) se(%9.6f) stats(N r2_within, labels("Obs." "R2 within"))

* quick read for 2d.b:
lincom mkvalt
display as txt "FE (Pay~MV): $ per $1k ΔMV = " as res %9.4f r(estimate)

