	/*--------------------------------------------------------------------------	
			Author: Seyed Mohsen Alavi
			Data: subscribers
			Last vesion : 2022 03 November 11 pm
	--------------------------------------------------------------------------*/


set more off

cd "/Users/htm/Desktop/econ/peaksa/"
//use "data/neighbour.dta", clear

use "data/merge_dahak_cleaned_01_5_17.dta", clear
	drop if zone==.
	replace nsms=1000*nsms

	gen x=xmean/10000
	gen y=ymean/100000

	egen group = group(sdate)

	tempfile temp1
	save `temp1',replace
	
	forvalues i=1/21 {
	use `temp1',clear
	keep if group == `i'
	nearstat y x , near(y x) distvar(dist) contvar(nsms unit) statvar(postnsms postunit) statname(sum) dband(0 5) nid(mv_feeder feeder) ncount(postnum)
	tempfile tem`i'
	save `tem`i'',replace
	}
	use `tem1',clear
	forvalues j=2/21 {
	append using `tem`j''
	}
	
	save "data/neighbourhood5.dta",replace
	
	forvalues i=1/21 {
	use `temp1',clear
	keep if group == `i'
	nearstat y x , near(y x) distvar(dist) contvar(nsms unit) statvar(postnsms postunit) statname(sum) dband(0 1) nid(mv_feeder feeder) ncount(postnum)
	tempfile tem`i'
	save `tem`i'',replace
	}
	use `tem1',clear
	forvalues j=2/21 {
	append using `tem`j''
	}
	
	save "data/neighbourhood1.dta",replace
	
	forvalues i=1/21 {
	use `temp1',clear
	keep if group == `i'
	nearstat y x , near(y x) distvar(dist) contvar(nsms unit) statvar(postnsms postunit) statname(sum) dband(0 2) nid(mv_feeder feeder) ncount(postnum)
	tempfile tem`i'
	save `tem`i'',replace
	}
	use `tem1',clear
	forvalues j=2/21 {
	append using `tem`j''
	}
	
	save "data/neighbourhood2.dta",replace
