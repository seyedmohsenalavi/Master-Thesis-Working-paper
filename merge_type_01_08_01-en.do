

	/*--------------------------------------------------------------------------	
			Author: Seyed Mohsen Alavi
			Data: merge(sms & feeder & weather & subs)
			Last vesion : 2022 03 November 11 pm
			steps of regression:
			step  1  |	Define Tempfiles
			step  2  |	Regression Per Unit
	--------------------------------------------------------------------------*/
	
set more off
cd "/Users/htm/Desktop/econ/peaksa/"

 set matsize 10000
//////////////////////////////////////////////////Define Tempfiles/////////////////////////////////////////

	use "data/mergestypenew4.dta", clear
	
//////////////////////////////////////////////////Message Type/////////////////////////////////////////
	
	gen hour=.
	replace hour=1 if shour==1
	replace hour=0 if shour==0
	
	gen incentive=.
	replace incentive=1 if sincentive==1
	replace incentive=0 if sincentive==0
	
	
	drop if sbusiness==1
	drop sbusiness
	replace sincentive=0 if sincentive==.
	
	drop if treatment==0
	
	qui  eststo: xtreg logcpu i.hour##i.incentive i.sdate, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather No
	test (1.hour#1.incentive=0) (1.hour#0.incentive=0) (0.hour#1.incentive=0)
	estadd scalar p_value = r(p)
	est store r87
	
	 qui  eststo: xtreg logcpu  i.hour##i.incentive i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	test (1.hour#1.incentive=0) (1.hour#0.incentive=0) (0.hour#1.incentive=0)
	estadd scalar p_value = r(p)
	est store r88
	
	esttab r87 r88 using "result/final/paneltype_en.tex",replace compress label b(%9.3f) se(%9.2f) star(* 0.1 ** 0.05 *** 0.01)  ///
	s(feeder date zoneweather p_value N, label("Feeder FE" "Date FE" "Zone*Weather FE" "coefficients equality p_value" "N. of Obs") fmt(%9.2f)) mtitles("FE" "FE") ///
	drop(*.sdate *.zone#*.temp *.zone#*.per *.zone#*.wspeed) title(" The Effect of SMS Message Type \label{paneltype}") ///
	longtable eqlabels(none) noomitted nobaselevels interaction(" x ") ///
	collabels(" ",lhs("Log CpU")) booktabs varlabels(1.hour "1.Time specific x 0.Regional incentive" 1.incentive "0.Time specific x 1.Regional incentive" 1.hour#1.incentive "1.Time specific x 1.Regional incentive" _cons "Constant") /// 
	addnotes("Notes: Zone*Weather FE: zone fixed effects mutilplied by weather" "features like temperature, pressure and wind's speed. SEs are Clusterd by Feeder.")
