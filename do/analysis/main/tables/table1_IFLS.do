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
  using "Table1_IFLS.tex", replace ///
  type booktabs ///
  collabels("Rural" "Urban" "Total") ///
	varlabels(r1 "Agriculture" r2 "Non-Agriculture" r3 "Number of Observations") ///
	nomtitles ///
	width(0.7\textwidth) ///
	substitute(333 \%)

restore
