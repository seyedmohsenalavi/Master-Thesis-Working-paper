

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
	
////////////////////////////////////////////////////////////////////////////////////////////////////	

/////////////////////////////////////////////////.....Panel reg.......///////////////////////////////

	use `temp1',clear

	qui  eststo: xtreg logcpu avg temp per wspeed i.dayofweek, fe vce(cluster code)
	estadd local feeder Yes
	estadd local dow Yes
	estadd local date No
	estadd local dh No
	estadd local zonedate No
	estadd local zoneweather No
	est store r51

	qui  eststo: xtreg logcpu avg i.sdate i.zone#i.sdate, fe vce(cluster code)
	estadd local feeder Yes
	estadd local dow No
	estadd local date Yes
	estadd local dh No
	estadd local zonedate Yes
	estadd local zoneweather No
	est store r52

		 
	qui  eststo: xtreg logcpu avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local dow No
	estadd local date Yes
	estadd local dh No
	estadd local zonedate No
	estadd local zoneweather Yes
	est store r53
	
	use `drop1',clear
	 
	 qui  eststo: xtreg logcpu avg i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	 estadd local feeder Yes
	estadd local dow No
	estadd local date Yes
	estadd local dh Yes
	estadd local zonedate No
	estadd local zoneweather Yes
	est store r54

	use `temp1',clear

	qui  eststo: xtreg logcpu i.treatment temp per wspeed i.dayofweek, fe vce(cluster code)
	estadd local feeder Yes
	estadd local dow Yes
	estadd local date No
	estadd local dh No
	estadd local zonedate No
	estadd local zoneweather No
	est store r55

	qui  eststo: xtreg logcpu i.treatment i.sdate i.zone#i.sdate, fe vce(cluster code)
	estadd local feeder Yes
	estadd local dow No
	estadd local date Yes
	estadd local dh No
	estadd local zonedate Yes
	estadd local zoneweather No
	est store r56

	 
	qui  eststo: xtreg logcpu i.treatment i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	estadd local feeder Yes
	estadd local dow No
	estadd local date Yes
	estadd local dh No
	estadd local zonedate No
	estadd local zoneweather Yes
	est store r57
	
	use `drop1',clear
	 
	 qui  eststo: xtreg logcpu i.treatment i.sdate i.zone#c.temp i.zone#c.per i.zone#c.wspeed, fe vce(cluster code)
	 estadd local feeder Yes
	estadd local dow No
	estadd local date Yes
	estadd local dh Yes
	estadd local zonedate No
	estadd local zoneweather Yes
	est store r58

	use `temp1',clear

	esttab r55 r56 r57 r58 r51 r52 r53 r54 using "result/final/reg1_en.tex",replace compress label b(%9.3f) se(%9.2f) star(* 0.1 ** 0.05 *** 0.01)  ///
	s(feeder dow date dh zonedate zoneweather N, label("Feeder FE" "DOW FE" "Date FE" "Drop weekend" "Zone*Date FE" "Zone*Weather FE" "N. of Obs") fmt(%9.0g)) mtitles("FE" "FE" "FE" "FE" "FE" "FE" "FE" "FE" "FE" "FE") ///
    keep(avg 1.treatment temp per wspeed) order(1.treatment avg temp wspeed per) title("Panel Regression \label{reg:panel1}") ///
	longtable eqlabels(none) noomitted nobaselevels varlabels(_cons "Constant" per "Pressure" wspeed "Wind speed" temp "Temperature" avg "Percent of Subs" 1.treatment "1.Treatment") ///
	collabels(" ",lhs("Log CpU")) booktabs ///
	addnotes("Notes: DOW: day of week, CpU: Consumption per Unit, Zone*Weather FE: zone fixed effects" "mutilplied by weather features like temperature, pressure and wind's speed." "Standard errors are Clusterd by Feeder.")


/////////////////////////////////////////////////.....Cross-section Matching.......///////////////////////////////
	
	global varlist1 "con1 con2 con3 con4 con5 con8 decile pertariff perphaze perheavy xmean ymean years_of_schooling wife_house_keeper"
	global varlist2 "decile pertariff perphaze perheavy xmean ymean years_of_schooling"
	global varlist3 "con0 decile pertariff perphaze perheavy xmean ymean years_of_schooling wife_house_keeper"
	global varlist4 "con1 con2 con3 con4 con5 con21 con22 con23 con24 con25 con26 con27 con28 con29 con30 con31 decile pertariff perphaze perheavy xmean ymean years_of_schooling wife_house_keeper"

////////////////////////////////////////////Treat1//////
*/
use `temp1',clear
	drop if period==2
	drop if treat==2
	
	forvalues i=21/31 {
	egen tempcon`i'=mean(logcpu) if period==0 & mday==`i' , by(mv_feeder)
	egen con`i'=mean(tempcon`i') , by(mv_feeder)
	drop tempcon`i'
	}
	
	forvalues i=1/5 {
	egen tempcon`i'=mean(logcpu) if period==0 & mday==`i' , by(mv_feeder)
	egen con`i'=mean(tempcon`i') , by(mv_feeder)
	drop tempcon`i'
	}
	
	
	forvalues j=6/8 {
	psmatch2 treat $varlist4 if mmonth==5 & mday==`j', outcome(logcpu) common logit quietly
	}
	psmatch2 treat $varlist4 if mmonth==5 & mday==11, outcome(logcpu) common logit quietly
	
	psgraph , treated(treat) pscore(_pscore) name(g1,replace) title("Treat1 and control group")
	//graph export "result/final/pscore1.png", replace
	
	teffects psmatch (logcpu) (treat $varlist3 ,logit) if mmonth==5 & mday==8, atet gen(ps1) nneighbor(1) 
	
	tebalance density ,name(te1_1,replace) title("Treat1 and control group")
	graph export "result/final/cs_te1.png", replace
	
	tebalance density ,name(te1,replace) title("T1 & C")

////////////////////////////////////////////Treat2//////
	use `temp1' ,clear
	
	drop if period==1
	drop if treat==1
	recode treat (2=1)
	
forvalues i=21/31 {
	egen tempcon`i'=mean(logcpu) if period==0 & mday==`i' , by(mv_feeder)
	egen con`i'=mean(tempcon`i') , by(mv_feeder)
	drop tempcon`i'
	}
	
	forvalues i=1/5 {
	egen tempcon`i'=mean(logcpu) if period==0 & mday==`i' , by(mv_feeder)
	egen con`i'=mean(tempcon`i') , by(mv_feeder)
	drop tempcon`i'
	}
	
	forvalues j=12/15 {
	psmatch2 treat $varlist4 if mmonth==5 & mday==`j', outcome(logcpu) common logit quietly
	}
	
	psgraph , treated(treat) pscore(_pscore) name(g2,replace) title("Treat2 and control group")
	//graph export "result/final/pscore2.png", replace
	
	teffects psmatch (logcpu) (treat $varlist4 ,logit) if mmonth==5 & mday==12, atet gen(ps2) //nneighbor(1) 
	
	tebalance density ,name(te2_1,replace) title("Treat2 and control group") 
	graph export "result/final/cs_te2.png", replace
	
	tebalance density ,name(te2,replace) title("T2 & C")

	graph combine g1 g2, name(g_cs,replace) title("Propensity score similarity between T. & C. groups")
	graph export "result/final/cs_g.png", replace
	
	graph combine te1 te2, name(te_cs,replace)
	graph export "result/final/cs_te.png", replace
*/
///////////////////////////////////////////////////DID-Matching/////////////////////////////////////////////

	global varlist5 "cons0 HeavySubs IncDecile Longitude Latitude Tariff Phaze years_of_schooling wife_house_keeper"
	
	use `temp1' ,clear
	drop if weekend==1
	label var treat treat
	
	drop con0
	
	egen tempcon=mean(logcpu) if period==0 , by(mv_feeder)
	egen con0=mean(tempcon) , by(mv_feeder)
	drop tempcon

	ren con0 cons0
	ren perheavy HeavySubs
	ren decile IncDecile
	ren	ymean Longitude
	ren	xmean Latitude
	ren pertariff Tariff
	ren perphaze Phaze
	
	drop if period==2
	drop if treat==2
	
	
	psmatch2 treat $varlist5 if mmonth==5 & mday==11, outcome(logcpu) common kernel logit quietly
	pstest $varlist5 ,both name(pst1,replace) title(Matching covariates bias between T1 & C) label graph
	graph export "result/final/pst1.png", replace
	
	egen psweight=mean(_weight) , by(mv_feeder)
	
	eststo:reg logcpu i.treat##i.period if (mmonth==5 & mday>=6 & mday<=11) | period==0 , vce(cluster code)
	est store r1
	
	eststo:reg logcpu i.treat##i.period c.cons0#i.period if (mmonth==5 & mday>=6 & mday<=11) | period==0 , vce(cluster code)
	est store r2
	
	eststo:reg logcpu i.treat##i.period [aweight=psweight] if (mmonth==5 & mday>=6 & mday<=11) | period==0 , vce(cluster code)
	est store r3
	
	use `temp1' ,clear
	drop if weekend==1
	label var treat treat
	
	drop con0
	
	egen tempcon=mean(logcpu) if period==0 , by(mv_feeder)
	egen con0=mean(tempcon) , by(mv_feeder)
	drop tempcon
	
	ren con0 cons0
	ren perheavy HeavySubs
	ren decile IncDecile
	ren	ymean Longitude
	ren	xmean Latitude
	ren pertariff Tariff
	ren perphaze Phaze
	
	drop if period==1
	drop if treat==1
	recode treat (2=1)
	
	psmatch2 treat $varlist5 if mmonth==5 & mday==12, outcome(logcpu) common logit kernel quietly
	pstest $varlist5 ,both graph name(pst2,replace) title(Matching covariates bias between T2 & C) label
	graph export "result/final/pst2.png", replace
	
	egen psweight=mean(_weight) , by(mv_feeder)
	recode treat (1=2)
	eststo:reg logcpu i.treat##i.period if (mmonth==5 & mday>=12 & mday<=15) | period==0 , vce(cluster code)
	est store r4
	
	eststo:reg logcpu i.treat##i.period c.cons0#i.period if (mmonth==5 & mday>=12 & mday<=15) | period==0 , vce(cluster code)
	est store r5
	
	eststo:reg logcpu i.treat##i.period [aweight=psweight] if (mmonth==5 & mday>=12 & mday<=15) | period==0 , vce(cluster code)
	est store r6
	
	esttab r1 r2 r3 r4 r5 r6 using "result/final/did_matching_en.tex",replace compress label b(%9.3f) se(%9.2f) star(* 0.1 ** 0.05 *** 0.01)  ///
	mtitles("DID" "DID" "PSM" "DID" "DID" "PSM")  noomitted nobaselevels longtable eqlabels(none)  ///
	collabels(,lhs("log(CpU)")) booktabs s(N ,label("N. of Obs") fmt(%9.0g)) ///
	addnotes("Notes: Cpu: Consumption per Unit. con0: log(Cons. per unit in period 0)." "Standard errors are Clusterd by Feeder.") ///
	title("DID Regression with Matching  \label{reg:didpsm}")
