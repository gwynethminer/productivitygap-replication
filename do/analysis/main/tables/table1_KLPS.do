********************************************************************************
*** Table 1: Non-Agriculture/Agriculture and Urban/Rural
********************************************************************************
preserve

lab var nonag " "
lab var urban "Urban dummy"
label define lab_nonag 0 "Agriculture" 1 "Non-Agriculture"
label define lab_urban 0 "Rural" 1 "Urban"
label values nonag lab_nonag
label values urban lab_urban

** Populate a matrix
matrix xtab = J(3,3,.)

* Rural column
sum nonag if urban==0
matrix xtab[1,1] = round((1-r(mean))*100, 0.1) + 0.0333
matrix xtab[2,1] = round(r(mean)*100, 0.1) + 0.0333
matrix xtab[3,1] = r(N)

* Urban column
sum nonag if urban==1
matrix xtab[1,2] = round((1-r(mean))*100, 0.1) + 0.0333
matrix xtab[2,2] = round(r(mean)*100, 0.1) + 0.0333
matrix xtab[3,2] = r(N)

* All column
sum nonag
matrix xtab[1,3] = round((1-r(mean))*100, 0.1) + 0.0333
matrix xtab[2,3] = round(r(mean)*100, 0.1) + 0.0333
matrix xtab[3,3] = r(N)


esttab matrix(xtab, fmt("%7.4f %7.4f %10.0fc" "%7.4f %7.4f %10.0fc"  "%7.4f %7.4f %10.0fc" )) ///
  using "$dtab/Table1_KLPS_PanelB.tex", replace ///
  type booktabs ///
  collabels("Rural" "Urban" "Total") ///
	varlabels(r1 "Agriculture" r2 "Non-Agriculture" r3 "Number of Observations") ///
	nomtitles ///
	width(0.7\textwidth) ///
	substitute(333 \%)
restore
********************************************************************************
preserve
use "$da/Consumption_Analysis_KLPS.dta", clear
*** Adding restrictions for SubsistenceAg
keep if !missing(urban)
keep if !missing(female)
keep if !missing(age)
keep if !missing(educyr)

drop nonag
gen nonag = .
replace nonag = 1 if nonaghour > aghour_notdecmaker & aghour_notdecmaker ~= . & nonaghour ~= .
replace nonag = 0 if nonaghour < aghour_notdecmaker & aghour_notdecmaker ~= . & nonaghour ~= .
replace nonag = 1 if nonaghour > 0 & nonaghour ~= . & aghour_notdecmaker == .
replace nonag = 0 if aghour_notdecmaker > 0 & nonaghour == . & aghour_notdecmaker ~= .

replace nonag = 0 if (abs(aghour_notdecmaker - nonaghour) < 10^-2) & aghour_notdecmaker ~= . & nonaghour ~= .
replace nonag = . if nonaghour == 0 & aghour_notdecmaker == 0
replace nonag = . if nonaghour == 0 & aghour_notdecmaker == .
replace nonag = . if nonaghour == . & aghour_notdecmaker == 0

keep if !missing(nonag)
keep if !missing(lninc_notdecmaker)
keep if !missing(lnhour_notdecmaker)
keep if !missing(lninc_h_notdecmaker)

gen sampleKLPS3 = .
replace sampleKLPS3 = 1 if yrmth_interview_KLPS3 - yrmth < 12
keep if sampleKLPS3  == 1

lab var nonag " "
lab var urban "Urban dummy"
label define lab_nonag 0 "Agriculture" 1 "Non-Agriculture"
label define lab_urban 0 "Rural" 1 "Urban"
label values nonag lab_nonag
label values urban lab_urban

** Populate a matrix
matrix xtab = J(3,3,.)

* Rural column
sum nonag if urban==0
matrix xtab[1,1] = round((1-r(mean))*100, 0.1) + 0.0333
matrix xtab[2,1] = round(r(mean)*100, 0.1) + 0.0333
matrix xtab[3,1] = r(N)

* Urban column
sum nonag if urban==1
matrix xtab[1,2] = round((1-r(mean))*100, 0.1) + 0.0333
matrix xtab[2,2] = round(r(mean)*100, 0.1) + 0.0333
matrix xtab[3,2] = r(N)

* All column
sum nonag
matrix xtab[1,3] = round((1-r(mean))*100, 0.1) + 0.0333
matrix xtab[2,3] = round(r(mean)*100, 0.1) + 0.0333
matrix xtab[3,3] = r(N)

esttab matrix(xtab, fmt("%7.4f %7.4f %10.0fc" "%7.4f %7.4f %10.0fc"  "%7.4f %7.4f %10.0fc" )) ///
  using "$dtab/Table1_KLPS_PanelC.tex", replace ///
  type booktabs ///
  collabels("Rural" "Urban" "Total") ///
	varlabels(r1 "Agriculture" r2 "Non-Agriculture" r3 "Number of Observations") ///
	nomtitles ///
	width(0.7\textwidth) ///
	substitute(333 \%)

restore
