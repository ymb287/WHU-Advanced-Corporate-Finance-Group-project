/******************************************************************************************
 APPENDIX A — STATA DO-FILE (Baseline Event Study)
 Purpose : Estimate market model and compute AR, CAR, AAR, CAAR for [-2, +2] event window
 Author  : Yannik Biebert, Jakob Gruss, Cayetano Llaca Kuri, Julian Vincent Müller, Victoria Walf
 Course  : Advanced Corporate Finance
 Notes   : Code for baseline window; same structure used for other event windows with
           adjusted estimation and event-window parameters. Before running this file, update
		   the working directory in Section 0 to match the local file path
******************************************************************************************/

*****************************************************
* 0) WORKING DIRECTORY
*****************************************************
* >>> UPDATE THIS PATH TO THE FOLDER CONTAINING THE EXCEL FILES <<< 
* The following line defines the working directory. 
* Please replace the path below with the folder on your computer 
* that contains the "Data-Acquisitions.xlsx" and "share price data.xlsx" files.

local workdir "C:\Your\Path\Here"
cd "`workdir'"

* Tempfiles for intermediate results
tempfile acq td prices events panel coefs workar car aart car_winsor

*****************************************************
* 1) ACQUISITIONS - IMPORT AND BASIC CLEANING
*****************************************************
import excel using "Data-Acquisitions.xlsx", firstrow clear
rename AnnouncementDate ann_raw
rename AcquirorTicker   ticker

* Convert ticker to string, strip spaces, and uppercase so values are consistent and the merge with the share-price data is smooth
tostring ticker, replace
replace  ticker = strupper(strtrim(ticker))

* Transform date from Excel to a clean Stata date (if necessary)
gen double ann_date = .
capture confirm numeric variable ann_raw
if _rc==0 {
    replace ann_date = dofd(ann_raw)
}
else {
    replace ann_date = date(ann_raw,"YMD") if missing(ann_date)
    replace ann_date = date(ann_raw,"MDY") if missing(ann_date)
    replace ann_date = date(ann_raw,"DMY") if missing(ann_date)
}
format ann_date %td

* Keep only ticker and date
keep ticker ann_date
order ticker ann_date

save "`acq'", replace

*****************************************************
* 2) SHARE PRICE DATA - IMPORT AND CLEANING
*****************************************************
* Rename variables
import excel using "share price data.xlsx", firstrow clear
rename Date          date_xl
rename Ticker        ticker
rename SharePrice    price
rename ReturnonSP500 mret

* Clean ticker so it's mergeable with the acquisitions file
tostring ticker, replace
replace  ticker = strupper(strtrim(ticker))

* Bring date into the same format as in the acquisitions file (as a precaution)
capture confirm numeric variable date_xl
if _rc==0 {
    gen date = dofd(date_xl)
}
else {
    gen date = date(date_xl,"YMD")
    replace date = date(date_xl,"DMY") if missing(date)
}
format date %td

* Split BRK into BRK.A and BRK.B using TICKER and PERMNO. Drop BRK.B and KMI
replace ticker = "BRK.A" if ticker == "BRK" & PERMNO == 17778
replace ticker = "BRK.B" if ticker == "BRK" & PERMNO == 83443
count if ticker == "BRK"
assert r(N) == 0   
drop if ticker=="BRK.B"
drop if ticker=="KMI"

gen firm = PERMNO

* Check for duplicates by firm and date. Only DELL had duplicates, which are dropped here
duplicates report firm date
duplicates list firm date
duplicates drop firm date, force

* Sort data by firm and date
sort firm date

* Set panel: firm identifier (PERMNO) and trading day
tsset firm date, daily

* Compute log returns for the firm and market
by firm (date): gen ret = ln(price/price[_n-1]) if _n > 1
replace mret = ln(1 + mret) if mret > -1 & mret < .

* Keep only firm, ticker, date, firm return, market return, and share price
keep firm ticker date ret mret price
save "`prices'", replace

*****************************************************
* 3) TRADING CALENDAR AND MAPPING ANNOUNCEMENTS TO THE NEAREST TRADING DAY
*****************************************************

* Create a list of all trading days from the price file for later mapping
use "`prices'", clear
keep date
duplicates drop
sort date
gen one = 1
save "`td'", replace

* Map each firm's announcement date to the trading calendar. If the date is not a trading day, shift to the next available trading day
use "`acq'", clear
rename ann_date ann_date_raw
gen one = 1
joinby one using "`td'"
drop one
keep if date >= ann_date_raw
bysort ticker ann_date_raw (date): keep if _n==1
rename date ann_date
format ann_date %td
egen event_id = group(ticker ann_date)
save "`events'", replace

* Check which announcements were shifted
preserve
    gen shifted = ann_date != ann_date_raw
	list ticker ann_date_raw ann_date if shifted
restore	

*****************************************************
* 4) JOIN PRICES AND EVENTS; DEFINE WINDOWS
*****************************************************

* Create a trading-day dataset from the price data
preserve
    use "`prices'", clear
    keep ticker date
    duplicates drop
    rename date ann_date
    tempfile traded
    save `traded'
restore

* Filter events to valid trading days (DELL drops here)
use "`events'", clear
joinby ticker ann_date using `traded'   
save "`events'", replace

* Merge acquisition events with full price series
use "`prices'", clear
joinby ticker using "`events'"

* Sort observations within event by date
sort event_id date

* Index trading days relative to the announcement date
by event_id: gen long seq  = _n

* Compute relative time (tau): days before/after announcement 
by event_id: egen long seq0 = total(cond(date==ann_date, seq, .)) 
gen int tau = seq - seq0

* Define estimation and event windows
gen byte in_est   = inrange(tau, -250, -50)
gen byte in_event = inrange(tau,   -2,    2)

* Keep only the relevant observations (in estimation OR event window)
keep if in_est | in_event
save "`panel'", replace

*****************************************************
* 5) MARKET MODEL PER EVENT (ESTIMATION WINDOW ONLY) & COMPUTE AR/CAR/AAR/CAAR
*****************************************************
use "`panel'", clear

* Estimate α and β for each event in the estimation window via `statsby`; store coefficients and diagnostics in `coefs`
preserve
    keep if in_est
    statsby _b _se e(N) e(df_m) e(df_r) e(r2) e(rss) e(mss) e(tss) e(rmse), ///
        by(event_id ticker ann_date) saving("`coefs'", replace): ///
        regress ret mret
restore

* Merge α and β into the event-panel
merge m:1 event_id using "`coefs'", nogen
rename (_b_cons _b_mret) (alpha beta)

* Compute expected returns and abnormal returns
gen exp_ret = alpha + beta*mret if in_event
gen ar      = ret   - exp_ret   if in_event

* Calculate Cumulative Abnormal Returns (CAR) for each event 
preserve
    keep if in_event
    collapse (sum) CAR=ar, by(firm event_id ticker ann_date)
    save "`car'", replace
restore

* Calculate Average Abnormal Returns (AAR) and Cumulative Average Abnormal Returns (CAAR). Abnormal returns are averaged across all events for each day in the event time (τ). Cumulative average abnormal return is obtained as the cumulative sum of AARs over the event window.
preserve
    keep if in_event
    collapse (mean) AAR=ar (semean) se=ar (count) N=ar, by(tau)
    sort tau
    gen CAAR  = sum(AAR)
	gen var_AAR  = se^2
    gen se_CAAR  = sqrt(sum(var_AAR))
    gen t_CAAR   = CAAR / se_CAAR
	gen t_AAR    = AAR / se
	order tau AAR se t_AAR CAAR se_CAAR t_CAAR N
    drop var_AAR
    save "`aart'", replace
restore

*****************************************************
* 6) RESULTS TABLES
*****************************************************

* ---------- Table 1: Cross-sectional CAAR test [-2,+2] ----------

* Load event-level CARs and compute cross-sectional mean CAAR
use "`car'", clear
collapse (mean) CAAR=CAR (sd) s=CAR (count) N=CAR

* Compute standard error, t-statistic, degrees of freedom, and p-value
gen se     = s / sqrt(N)
gen t_CAAR = CAAR / se
gen df     = N - 1
gen p      = 2 * ttail(df, abs(t_CAAR))

* Round N and df for presentation; format to six decimals
replace df = round(df)
replace N  = round(N)

* Format to six decimals
format CAAR se t_CAAR p %12.6f
format df N %9.0f

* Display results as Table 1: "Cross-sectional Test of CAAR [-2, +2]"
di as txt "-----------------------------------------------------------"
di as txt "Table 1. Cross-sectional Test of CAAR [-2,+2]"
di as txt "-----------------------------------------------------------"
tabstat CAAR se t_CAAR df p N, stat(mean) columns(variables) format(%12.6f)
di as txt " "


* ---------- Table 2: AAR & CAAR by Event Day ----------

* Load AAR/CAAR dataset and format variables for presentation
use "`aart'", clear

* Report daily AAR and CAAR, including SEs, t-statistics, and N by event day (τ)
format AAR se t_AAR CAAR se_CAAR t_CAAR %12.6f
format N %9.0f

* Display results as Table 2: "Average Abnormal Returns (AAR) and CAAR by event day"
di as txt "-----------------------------------------------------------"
di as txt "Table 2. Average Abnormal Returns (AAR) and CAAR by Event Day"
di as txt "-----------------------------------------------------------"
tabstat AAR se t_AAR CAAR se_CAAR t_CAAR N, by(tau) nototal columns(variables) format(%12.6f)
di as txt " "


* ---------- Table 3: Market Model Summary (alpha, beta, R², adjR²) ----------

* Load market-model coefficients estimated in the estimation window
use "`coefs'", clear

* Extract α and β; compute R² and adjusted R²
gen alpha = _b_cons
gen beta  = _b_mret
gen r2    = _eq2_stat_4
gen N     = _eq2_stat_1
local k = 1
gen adjR2 = 1 - (1 - r2) * ((N - 1) / (N - `k' - 1))

* Format results and round N for presentation consistency
replace N = round(N)
format alpha beta r2 adjR2 %12.6f
format N %9.0f

* Display results as Table 3: "Market Model Summary (alpha, beta, R², adjR²)"
di as txt "-----------------------------------------------------------"
di as txt "Table 3. Market Model Summary (alpha, beta, R², adjR²)"
di as txt "-----------------------------------------------------------"
tabstat alpha beta r2 adjR2, stats(mean p50 min max sd n) columns(variables) format(%12.6f)
di as txt " "

*****************************************************
* 7) WINSORIZED ANALYSIS (5% / 95%)
*****************************************************

* Load CAR dataset and create winsorized version of CARs
use "`car'", clear
quietly summarize CAR, detail
gen CAR_w = CAR

* Replace values below the 5th and above the 95th percentile with the respective cutoffs to reduce the influence of outliers
replace CAR_w = r(p5)  if CAR < r(p5)
replace CAR_w = r(p95) if CAR > r(p95)

* Compute mean, SD, and N of winsorized CARs
collapse (mean) CAAR=CAR_w (sd) s=CAR_w (count) N=CAR_w

* Compute SE, t-statistic, df, and p-value for the cross-sectional test of mean CAAR
gen se     = s / sqrt(N)
gen t_CAAR = CAAR / se
gen df     = N - 1
gen p      = 2 * ttail(df, abs(t_CAAR))

* Round df and N; format all variables for display
replace df = round(df)
replace N  = round(N)
format CAAR se t_CAAR p %12.6f
format df N %9.0f

* Report results as Table 4: "Winsorized CAAR (5th/95th Percentile)"
di as txt "-----------------------------------------------------------"
di as txt "Table 4. Winsorized CAAR (5th/95th Percentile)"
di as txt "-----------------------------------------------------------"
tabstat CAAR se t_CAAR df p N, stat(mean) columns(variables) format(%12.6f)
di as txt " "

****************************************************
* 8) VISUALIZATION — BOXPLOTS OF EVENT-LEVEL CARs
****************************************************

* ---------- 8a) Event-level Boxplot (raw CARs) ----------
* Visualize the distribution of CARs across events with TICKER and announcement date on the x-axis; shows firm-level variation and potential outliers

use "`car'", clear

* Build readable event label (TICKER + YYYY-MM-DD)
gen str evlbl = ticker + " (" + string(ann_date, "%tdCCYY-NN-DD") + ")"

* Encode label as numeric for plotting
encode evlbl, gen(event)

* Create boxplot of CARs by event
graph box CAR, over(event, label(angle(45) labsize(vsmall))) ///
    title("CAR by Event") ytitle("Cumulative Abnormal Return") ///
    name(CAR_by_event, replace)

* Export figure as PNG
graph export "`workdir'\CAR_boxplot_by_event.png", replace


* ---------- 8b) Event-level Boxplot (winsorized CARs, 5th/95th) ----------
* Repeat the same visualization using winsorized CARs to illustrate the effect of outlier adjustment on the CAR distribution

preserve
use "`car'", clear

* Obtain percentiles and winsorize
quietly summarize CAR, detail
gen CAR_w = CAR
replace CAR_w = r(p5)  if CAR < r(p5)
replace CAR_w = r(p95) if CAR > r(p95)

* Build event labels and encode
gen str evlbl = ticker + " (" + string(ann_date, "%tdCCYY-NN-DD") + ")"
encode evlbl, gen(event)

* Create boxplot of winsorized CARs by event
graph box CAR_w, over(event, label(angle(45) labsize(vsmall))) ///
    title("CAR by Event (Winsorized 5%/95%)") ///
    ytitle("Cumulative Abnormal Return") ///
    name(CAR_by_event_winsor, replace)

* Export figure as PNG
graph export "`workdir'\CAR_boxplot_by_event_winsor_5_95.png", replace
restore
