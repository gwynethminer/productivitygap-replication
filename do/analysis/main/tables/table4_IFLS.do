********************************************************************************
*** Table 4: Non-Agricultural/Agricultural Gap in Earnings
********************************************************************************
*Column 1: OLS, raw gap
reg lninc nonag, cluster(pidlink)
estadd local timeFE "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A

*Column 2: time fixed effects and controls for log hours and log hours squared
reg lninc nonag lnhour lnhour_sq i.year, cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store B

*Column 3: all adjustments from Col 2 plus indicator for female, education yrs, edu yrs sqrd
reg lninc nonag lnhour lnhour_sq female age age_sq educyr educyr_sq i.year, cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C

*Column 4: all adjustments from Col 3 plus scores from Raven's Matrices tests
reg lninc nonag lnhour lnhour_sq female age age_sq educyr educyr_sq ravens_norm ravens_norm_sq i.year, cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store D

*Column 5: all adjustments from Col 3 and limit sample to those that have productivity measures in ag and nonag
reg lninc nonag lnhour lnhour_sq female age age_sq educyr educyr_sq i.year if NA_mover == 1, cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local moverUonly "Y" , replace
estadd local cluster `e(N_clust)', replace
est store E

*Column 6: individual FE, time FE, no hours control
xtreg lninc nonag age_sq i.year, fe i(pidlink) cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store F

*Column 7: individual FE, time FE, log hours and log hours sqrd
xtreg lninc nonag lnhour lnhour_sq age_sq i.year, fe i(pidlink) cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G

*Column 8: using wage as dependent var (tot earnings / hrs worked), same specifications as Col 6
xtreg lninc_h nonag age_sq i.year, fe i(pidlink) cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store H

*Column 9: same specification as Col 8, adjusting for urban prices
xtreg lninc_h_real nonag age_sq i.year, fe i(pidlink) cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store I

esttab A B C D E F G H I using "results/tables/Table4_IFLS.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(S S) collabels(none) ///
	drop(_cons *year* age age_sq) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "" "" "" "" "" "Log Wage" "Log Real Wage")  ///
	mgroups("Dependent variable: Log Earnings", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE timeFE moverUonly N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Time fixed effects"' `"Switchers only"' `"Number of observations"' `"Number of individuals"'))
