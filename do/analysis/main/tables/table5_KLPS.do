********************************************************************************
*** Table 5: Urban/Rural Gap in Earnings
********************************************************************************
*** lninc_urban_KLPS_extraC6
reg lninc urban [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lninc urban lnhour lnhour_sq i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store B
reg lninc urban lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
reg lninc urban lnhour lnhour_sq female age age_sq educyr educyr_sq ravens ravens_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store D
reg lninc urban lnhour lnhour_sq female age age_sq educyr educyr_sq ravens ravens_sq voced voced_treat_info voced_treat_voucher i.yrmth if U_mover == 1 [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local moverUonly "Y" , replace
estadd local cluster `e(N_clust)', replace
est store E
xtreg lninc urban age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store F
xtreg lninc urban lnhour lnhour_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G
xtreg lninc_h urban age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store H
xtreg lninc_h_real urban age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store I

esttab A B C D E F G H I using "$dtab/Table5_KLPS.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(c c) collabels(none) /// // alignment(S S)  // collabels("\multicolumn{1}{c}{$\beta$ / SE}" // drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	drop(_cons *yrmth* voced voced_treat_info voced_treat_voucher age age_sq) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "" "" "" "" "" "Log Wage" "Log Real Wage")  ///
	mgroups("Dependent variable: Log Earnings", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE timeFE moverUonly N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Time fixed effects"' `"Switchers only"' `"Number of observations"' `"Number of individuals"'))
