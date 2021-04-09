********************************************************************************
*** Table 3: Correlates of Employment in Non-Agriculture and Urban Migration
********************************************************************************
preserve

*** IFLS analysis
use "$da/Table3_IFLS.dta", clear
rename ravens_norm ravens		// Only to fix labels
reg nonag_ever educpri educsec educcol female ravens if always_rural==1 | migrRU_ever==1, robust
est store A
reg migrRU_ever educpri educsec educcol female ravens if always_rural==1 | migrRU_ever==1, robust
est store C

restore

*** KLPS analysis
reg nonag_ever educpri educsec educcol female ravens, robust
est store B
reg migrU_ever educpri educsec educcol female ravens, robust
est store D

esttab A B C D using "results/tables/Table3.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(S S S S) collabels(none) ///	// drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mgroups("\centering\shortstack{Dependent Variable:\\Non-Agricultural Employment}" "\centering\shortstack{Dependent Variable:\\Urban Migration}", pattern(1 0 1 0) prefix(\multicolumn{@span}{p{5.5cm}}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	mtitle("Indonesia" "Kenya" "Indonesia" "Kenya")  /// // "Movers" "Urban Movers"
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///	//	stats(N, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"'))
	stats(N, fmt(0) ///
	labels(`"Observations"'))
