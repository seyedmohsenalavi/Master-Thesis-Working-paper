

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

use "data/merge_dahak_cleaned_01_5_17.dta", replace

	gen treatment=0
	replace treatment=1 if avg>0.1
	
	egen tempcon=mean(logcpu) if period==0 , by(mv_feeder)
	egen con0=mean(tempcon) , by(mv_feeder)
	drop tempcon
	
	tempfile temp1
	save `temp1', replace

	drop if weekend==1
	
	tempfile drop1
	save `drop1', replace

//////////////////////////////////////////////////Temporal spillover/////////////////////////////////////////
	
	use `temp1',clear
	forvalues n=1/4 {
	gen lag`n'_avg=l`n'.avg
	label var lag`n'_avg "Lag`n' Percent of Subs"
	}

	qui  eststo: xtreg logcpu avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh No
	est store r1

	 qui  eststo: xtreg logcpu avg lag1_avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh No
	est store r2

	 qui  eststo: xtreg logcpu avg lag1_avg lag2_avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh No
	est store r3
	
	use `drop1',clear
	forvalues n=1/4 {
	gen lag`n'_avg=l`n'.avg
	label var lag`n'_avg "Lag`n' Percent of Subs"
	}
	
	qui  eststo: xtreg logcpu avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh Yes
	est store r4

	 	qui  eststo: xtreg logcpu avg lag1_avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh Yes
	est store r5

	 	qui  eststo: xtreg logcpu avg lag1_avg lag2_avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh Yes
	est store r6

	use `temp1',clear
	forvalues n=1/2 {
	gen lag`n'_avg=l`n'.treatment
	label var lag`n'_avg "Lag`n' of 1.Treat"
	}

	qui  eststo: xtreg logcpu i.treatment i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh No
	est store r7

	 qui  eststo: xtreg logcpu  i.treatment lag1_avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh No
	est store r8

	 qui  eststo: xtreg logcpu  i.treatment lag1_avg lag2_avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh No
	est store r9
	
	use `drop1',clear
	forvalues n=1/2 {
	gen lag`n'_avg=l`n'.avg
	label var lag`n'_avg "Lag`n' of 1.Treat"
	}
	
	qui  eststo: xtreg logcpu  i.treatment i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh Yes
	est store r10

	 	qui  eststo: xtreg logcpu  i.treatment lag1_avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh Yes
	est store r11

	 	qui  eststo: xtreg logcpu  i.treatment lag1_avg lag2_avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh Yes
	est store r12

	esttab r7 r10 r8 r11 r9 r12 using "result/final/panel_lag2-en.tex",replace compress label b(%9.3f) se(%9.2f) star(* 0.1 ** 0.05 *** 0.01)  ///
	s(feeder date zoneweather dh N, label("Feeder FE" "Date FE" "Zone*Weather FE" "Drop weekends" "N. of Obs") fmt(%9.0g)) mtitles("FE" "FE" "FE" "FE" "FE" "FE" "FE" "FE" "FE" "FE") ///
    keep(1.treatment lag1_avg  lag2_avg) title(" The Temporal Effect of SMS Message   \label{reg:temporal2}") ///
	longtable eqlabels(none) noomitted nobaselevels varlabels(_cons "Constant" 1.treatment "1.Treat") ///
	collabels(" ",lhs("Log(CpU)")) booktabs ///
	addnotes("Notes: Zone*Weather FE: zone fixed effects mutilplied by weather features like" "temperature, pressure and wind's speed. Standard errors are Clusterd by Feeder." "Lag1 of treat means one day before, treat has been occured.")
	
	

//////////////////////////////////////////////////Neighborhood spillover/////////////////////////////////////////
	
	use `temp1',clear
	replace nsms=nsms*1000
	
	//replace unit=1 if  unit==.
	
	egen postnsms=total(nsms) , by(postcode sdate)
	egen postunit=total(unit) , by(postcode sdate)
	replace postnsms=postnsms-nsms
	replace postunit=postunit-unit
	
	gen postavg=postnsms/postunit
	label var postavg "Percent of neighbour subs"
	
	qui  eststo: xtreg logcpu avg postavg i.sdate, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather No
	estadd local dh No
	est store r71

		qui  eststo: xtreg logcpu avg postavg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh No
	est store r72

	use `drop1',clear
	replace nsms=nsms*1000
	
	//replace unit=1 if  unit==.
	
	egen postnsms=total(nsms) , by(postcode sdate)
	egen postunit=total(unit) , by(postcode sdate)
	replace postnsms=postnsms-nsms
	replace postunit=postunit-unit
	
	gen postavg=postnsms/postunit
	label var postavg "Percent of neighbour subs"
	
	  	qui  eststo: xtreg logcpu avg postavg i.sdate, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather No
	estadd local dh Yes
	est store r73


		qui  eststo: xtreg logcpu avg postavg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
			estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh Yes
	est store r74


*/
use "data/neighbourhood1.dta",clear
	
	drop if mod(zone,1)!=0
	label var avg "Percent of subs"
		replace postnsms=postnsms-nsms
	replace postunit=postunit-unit
	gen postavg=postnsms/postunit
	label var postavg "Percent of neighbour subs"
	
	tempfile temp3
	save `temp3'

	qui  eststo: xtreg logcpu avg  postavg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh No
	est store r78

	use `temp3',clear
	drop if weekend==1

	qui  eststo: xtreg logcpu avg  postavg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh Yes
	est store r84

	
use "data/neighbourhood5.dta",clear
	
	drop if mod(zone,1)!=0
		label var avg "Percent of subs"
		replace postnsms=postnsms-nsms
	replace postunit=postunit-unit
	gen postavg=postnsms/postunit
	label var postavg "Percent of neighbour subs"
	
	tempfile temp4
	save `temp4'

		qui  eststo: xtreg logcpu avg  postavg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
				estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh No
	est store r79

	use `temp4',clear
	drop if weekend==1


		qui  eststo: xtreg logcpu avg  postavg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
					estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh Yes
	est store r85

use "data/neighbourhood2.dta",clear
	
	drop if mod(zone,1)!=0
	label var avg "Percent of subs"
	replace postnsms=postnsms-nsms
	replace postunit=postunit-unit
	gen postavg=postnsms/postunit
	label var postavg "Percent of neighbour subs"
	
	tempfile temp5
	save `temp5'

		qui  eststo: xtreg logcpu avg postavg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
				estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh No
	est store r80

	use `temp5',clear
	drop if weekend==1

		qui  eststo: xtreg logcpu avg postavg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local date Yes
	estadd local zoneweather Yes
	estadd local dh Yes
	est store r86


		esttab r73 r74 r78 r80 r79 r84 using "result/final/panel_neighbour4-en.tex",replace compress label b(%9.3f) se(%9.2f) star(* 0.1 ** 0.05 *** 0.01)  ///
	s(feeder date zoneweather dh N, label("Feeder FE" "Date FE" "Zone*Weather FE" "Drop weekends" "N. of Obs") fmt(%9.0g)) mtitles("Post" "Post" "1KM" "1KM" "2KM" "2KM") ///
    keep(avg postavg) title("The Neighborhood Effect of SMS Message     \label{panelneighbour4}") ///
	longtable eqlabels(none) noomitted nobaselevels varlabels(_cons "Constant") ///
	collabels(" ",lhs("Log(CpU)")) booktabs ///
	addnotes("Notes: Zone*Weather FE: zone fixed effects mutilplied by weather features like" "temperature, pressure and wind's speed. In first 2 column neighbourhood" "is defined by feeders whose electricity post are the same.")
