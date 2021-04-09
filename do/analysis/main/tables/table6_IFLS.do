********************************************************************************
*** Table 6: Gap in Earnings, Indonesia. For Individuals Born Rural and Urban
********************************************************************************
*** Panel A
preserve
*** Born in rural area
keep if urban_birth == 0

reg lninc urban, cluster(pidlink)
estadd local timeFE "N" , replace
estadd local indFE "N" , replace
estadd local contr "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lninc urban lnhour lnhour_sq female age age_sq educyr educyr_sq i.year, cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local contr "Y" , replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg lninc_h urban age_sq i.year, fe i(pidlink) cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local contr "Y" , replace
estadd local cluster `e(N_clust)', replace
est store H

esttab A C H using "results/tables/Table6_IFLS_rural.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(S S) collabels(none) ///
	drop(_cons lnhour lnhour_sq female age age_sq educyr educyr_sq age_sq *year*) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "Log Wage")  ///
	mgroups("Dependent variable: Log Earnings", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE contr N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Number of observations"' `"Number of individuals"'))

restore


********************************************************************************
*** Panel B
preserve
*** Born in urban area
keep if urban_birth == 1

reg lninc urban, cluster(pidlink)
estadd local timeFE "N" , replace
estadd local indFE "N" , replace
estadd local contr "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lninc urban lnhour lnhour_sq female age age_sq educyr educyr_sq i.year, cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local contr "Y" , replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg lninc_h urban age_sq i.year, fe i(pidlink) cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local contr "Y" , replace
estadd local cluster `e(N_clust)', replace
est store H

esttab A C H using "Table6_IFLS_urban.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(S S) collabels(none) ///
	drop(_cons lnhour lnhour_sq female age age_sq educyr educyr_sq age_sq *year*) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "Log Wage")  ///
	mgroups("Dependent variable: Log Earnings", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE contr N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Number of observations"' `"Number of individuals"'))

restore
