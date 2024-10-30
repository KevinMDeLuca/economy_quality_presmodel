////////////////////////
/*
Fair Model + DeLuca Quality Measure
Economy + Quality Model
Predict the 2024 results

10/29/24
Kevin DeLuca
*/
////////////////////////


*first, get "relative partisanship" across states, using poll data:
import delimited ../data/president_polls.csv, clear //downloaded 10/29

*differences in averages by method*
tab methodology

bysort question_id: egen trows = count(question_id)
keep if trows == 2
drop trows

keep if candidate_name=="Kamala Harris"|candidate_name=="Donald Trump"|candidate_name=="Joe Biden"
keep poll_id pollster_id pollster sponsor_ids sponsors display_name pollster_rating_id pollster_rating_name numeric_grade pollscore methodology transparency_score end_date question_id sample_size population cycle office_type party candidate_id candidate_name pct  answer state

drop party candidate_id candidate_name
duplicates drop

rename pct pct_
reshape wide pct_, i(question_id) j(answer) str

gen temp = pct_Biden!=.&pct_Trump!=.
replace temp = 1 if pct_Harris!=.&pct_Trump!=.
drop if temp==0
drop temp

gen temp = substr(end_date, strpos(end_date,"/")+1,.)
gen temp2 = substr(temp, strpos(temp,"/")+1,.)
keep if temp2=="24"
drop temp2
gen year = 2024
gen day = substr(temp,1,strpos(temp,"/")-1)
gen month = substr(end_date,1,strpos(end_date,"/")-1)

destring(month day year), replace
*generate a Stata date variable from day, month, and year
gen date_var = mdy(month, day, year)

*format the new date variable to display as a readable date
format date_var %td

gen week = week(date_var)

drop temp year day month date_var
drop if population=="a"

*gen d_spread - dem margin in the poll
*first, convert to two-party vote shares
gen tvote = pct_Biden+pct_Trump
replace tvote = pct_Harris+pct_Trump if tvote==.|week>=30
gen d2pvote = pct_Biden/tvote
replace d2pvote = pct_Harris/tvote if week>=30 // use Harris v Trump as margin starting week 30 (after biden drops out)
replace d2pvote = d2pvote*100
gen r2pvote = 100-d2pvote
gen d_spread = d2pvote-r2pvote
label variable d_spread "Dem Margin"

compress
save ../data/cleanedpolls.dta, replace


*more complext way: predict partisanship by state (incl national) 
use ../data/cleanedpolls.dta, clear
keep if week>=36

replace state="National" if state==""
encode state, generate(state_g)

*state fixed effects
reghdfe d_spread if numeric_grade>=2&numeric_grade!=.&population=="lv", a(state_g, savefe) 
rename __hdfe1__ state_fe

*poll sample stats...
tab pollster if e(sample)
tab state if e(sample)

*clean
keep state state_g state_fe
drop if state_fe==.
duplicates drop 
sort state_g
gen temp = state_fe if state=="National"
egen temp2 = mean(temp)
gen relative_partisanship = (state_fe-temp2)/2
drop temp*
drop if state=="National"

save ../data/relativepartisanship_bystate.dta, replace

*table of relative partisanship for swing states
statastates, n(state)
keep if _merge==3
drop _merge
keep if state_abbrev=="AZ"|state_abbrev=="GA"|state_abbrev=="MI"|state_abbrev=="NV"|state_abbrev=="NC"|state_abbrev=="PA"|state_abbrev=="WI"
order state_g relative_partisanship



*Economy + Quality Model
clear
set more off
graph drop _all

*fair model data
import delimited "../data/atbl1.txt", delimiter(space, collapse) varnames(1) clear
dropmiss, force
preserve
drop in 38/75

rename t year
destring(year vp vc i dper dur war g p z), replace force

tempfile pt1
save `pt1'
restore

drop in 1/38
rename vp vcc
rename war zcc
rename i war
rename vc i
rename dper gcc
rename dur pcc
dropmiss, force
rename t year
destring(_all), replace force

*presidential quality differential - pres only datasubset from quality paper
import delimited "../data/presonly.csv", clear
keep year fe_election
tempfile presq
save `presq'

*Fair Model Estimate, with quality
use `pt1', clear
merge 1:1 year using `presq'

*add row for 2024
set obs 38
replace year = 2024 in 38
replace i = 1 in 38
replace dper = 0 in 38
replace dur = 0 in 38
replace war = 0 in 38
replace g = 1.7 in 38 //from current fair model (july 25)
replace p = 4.54 in 38 //from current fair model (july 25)
replace z = 4 in 38 //from current fair model (july 25)

*assume 0.4 quality advantage for Harris
replace fe_election = 0.4 in 38

*some more variables
gen g_i = g*i
gen p_i = p*i
gen z_i = z*i

*labels
label variable fe_election "Quality Differential"
label variable g_i "G*I"
label variable p_i "P*I"
label variable z_i "Z*I"

*estimate and predictions, both models

*original fair model
reg vp g_i p_i z_i i dper dur war if year>=1916
outreg2 using "../results/modelstable.tex", tex(frag) replace label keep(g_i p_i z_i i dper dur war) adjr2 nocons dec(2) nonotes ctitle("Original Fair Model")  addtext(Sample, 1916-2020, MAE, 2.11, RMSE, 2.53, 2024 Prediction, 49.3)
predict vp_pred_fair, xb 
label variable vp_pred_fair "Fair Model Pred"

preserve
predict vp_pred_fair_r, resid
replace vp_pred_fair_r = vp_pred_fair_r*-1
keep if year>=1952
graph twoway (scatter vp_pred_fair_r fe_election) (lfit vp_pred_fair_r fe_election), ylab(-8(2)8) ytitle("Fair Model Residuals") xtitle("Candidate Quality Differential") legend(off)
graph export "../results/fairresidquality.pdf", replace
restore

*Economy + Quality Model
reg vp g_i p_i z_i i dper dur war fe_election if year>=1952
reg vp vp_pred_fair fe_election 
outreg2 using "../results/modelstable.tex", tex(frag) label keep(vp_pred_fair fe_election) adjr2 nocons dec(2) nonotes ctitle("Fair Model with Quality")  addtext(Sample, 1952-2020, MAE, 1.14, RMSE, 1.59, 2024 Prediction, 52.5)
predict vp_pred_deluca, xb 


*compare errors
summarize vp_pred_fair vp_pred_deluca if year>=1952

gen error_fair = abs(vp_pred_fair-vp) if year>=1916
gen error_deluca = abs(vp_pred_deluca-vp) if year>=1952

summarize error_fair error_deluca 

*RMSE
gen error_fair_rmse = error_fair*error_fair
gen error_deluca_rmse = error_deluca*error_deluca
egen error_fair_rmse_mean = mean(error_fair_rmse)
egen error_deluca_rmse_mean = mean(error_deluca_rmse)
replace error_fair_rmse = sqrt(error_fair_rmse_mean)
replace error_deluca_rmse = sqrt(error_deluca_rmse_mean)

summarize error_fair_rmse error_deluca_rmse 

*comparison plot
drop if year<1952
graph twoway (scatter vp year, msymbol(o)) (scatter vp_pred_fair year, mcolor(red)) (scatter vp_pred_deluca year, mcolor(green)), legend(on pos(6) rows(2) label(1 "Actual") label(2 "Fair Model") label(3 "Economy (Fair Model) + Candidate Quality")) ytitle("Two-Party Democratic Vote Shares") xtitle("Election Year") xlab(1952(4)2024, angle(40)) ylab(35(5)65)
graph export "../results/fairplusquality.pdf", replace

*table of actual and predicted values, by year
keep year fe_election vp vp_pred_fair vp_pred_deluca



*now do electorl college prediction, using relative partisanship from before
use ../data/relativepartisanship_bystate.dta, clear

gen fairdeluca_pred = 52.5 //prediction from economy+quality model (0.4 quality differential)
gen stateprediction = fairdeluca_pred+relative_partisanship

statastates, n(state)
keep if _merge==3
drop _merge
keep if state_abbrev=="AZ"|state_abbrev=="GA"|state_abbrev=="MI"|state_abbrev=="NV"|state_abbrev=="NC"|state_abbrev=="PA"|state_abbrev=="WI"

*state-level vote share predictions
gen stateprediction_rounded = round(stateprediction, 0.01)
graph bar stateprediction_rounded, over(state_abbrev) ytitle("Predicted Democratic Vote Share") yline(50) ylab(49(1)53) exclude0 blabel(bar) 
graph export "../results/swingstatepredictions.pdf", replace

*dem margin predictions
gen d_margin = round((stateprediction-50)*2, 0.1)
graph bar d_margin, over(state_abbrev) ytitle("Predicted Democratic Margin") yline(0) ylab(-1(2)5) exclude0 blabel(bar) 
graph export "../results/swingstatemargins.pdf", replace



