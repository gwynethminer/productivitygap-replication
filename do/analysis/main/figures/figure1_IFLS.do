********************************************************************************
*** Figure 1: Productivity Gap in Total Earnings
********************************************************************************
use "Main_Analysis_IFLS.dta", clear

reg lninc urban, cluster(pidlink)
estadd local timeFE "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lninc urban lnhour lnhour_sq female age age_sq educyr educyr_sq i.year, cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg lninc urban lnhour lnhour_sq age_sq i.year, fe i(pidlink) cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store F
xtreg lninc_h urban age_sq i.year, fe i(pidlink) cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G

	preserve
	clear
	set obs 7
	gen num = _n
	gen coef = .
	gen sd = .
	gen pos_coef = 9

	estimates restore A
	replace coef = _b[urban] if num == 4
	replace sd = _se[urban] if num  == 4
	estimates restore C
	replace coef = _b[urban] if num == 5
	replace sd = _se[urban] if num  == 5
	estimates restore F
	replace coef = _b[urban] if num == 6
	replace sd = _se[urban] if num  == 6
	estimates restore G
	replace coef = _b[urban] if num == 7
	replace sd = _se[urban] if num  == 7
	format coef %9.3f
	gen column = num
	gen ci_hi = coef + 1.96 * sd
	gen ci_lo = coef - 1.96 * sd

	replace pos_coef = 9 in 6

	#delimit ;
	twoway

	(rcap ci_hi ci_lo column if inrange(num, 6, 7), lwidth(thick) lcolor(gs6) msize(zero))
	(scatter coef column if inrange(num, 6, 7),
		msymbol(diamond) msize(2.8) mfcolor(gs6) mlcolor(gs6)
		mlabel(coef) mlabc(black) mlabv(pos_coef) mlabg(1) mlabsize(3))

	(rcap ci_hi ci_lo column if inrange(num, 4,5) , lwidth(thick) lcolor(gs12) msize(zero))
	(scatter coef column if inrange(num, 4,5),
		msymbol(diamond) msize(2.8) mfcolor(gs12) mlcolor(gs12)
		mlabel(coef) mlabc(black) mlabv(pos_coef) mlabg(1) mlabsize(3)) ,
	yline(0, lcolor(gs5) lpattern(dash))
	xscale(range(3.5 6.5) off)
	yscale(range(-0.3 2.1) off)
	xsize(4)
	ysize(5)
	ylabel(0(0.5)2)
	fxsize(63.16)
	graphregion(color(white) margin(vlarge))
	legend(off)
	title("  C. Rural/Urban, Indonesia", size(3.7) position(11) color(black))
	text(2.05 5 "IFLS", place(c) size(3.6))
	name(ind_ru, replace)
	;
	 #delimit cr

	graph save "Figure1_IFLS_ruralurban.gph", replace

	restore

reg lninc nonag, cluster(pidlink)
estadd local timeFE "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lninc nonag lnhour lnhour_sq female age age_sq educyr educyr_sq i.year, cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg lninc nonag lnhour lnhour_sq age_sq i.year, fe i(pidlink) cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store F
xtreg lninc_h nonag age_sq i.year, fe i(pidlink) cluster(pidlink)
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G

	preserve

	clear
	set obs 7
	gen num = _n
	gen coef = .
	gen sd = .
	gen pos_coef = 9

	replace coef = 1.381 if num == 1
	replace sd = 0 if num==1
	replace coef = 1.075 if num == 2
	replace sd = 0 if num==2
	replace num = 1.15 if num==1
	replace num = 1.85 if num==2

	estimates restore A
	replace coef = _b[nonag] if num == 4
	replace sd = _se[nonag] if num  == 4
	estimates restore C
	replace coef = _b[nonag] if num == 5
	replace sd = _se[nonag] if num  == 5
	estimates restore F
	replace coef = _b[nonag] if num == 6
	replace sd = _se[nonag] if num  == 6
	estimates restore G
	replace coef = _b[nonag] if num == 7
	replace sd = _se[nonag] if num  == 7
	format coef %9.3f
	gen column = num
	gen ci_hi = coef + 1.96 * sd
	gen ci_lo = coef - 1.96 * sd

	gen label = ""
	replace label = "Raw" in 1
	replace label = "Adjusted" in 2
	replace label = "Raw" in 4
	replace label = "Adjusted" in 5
	replace label = "FE, earnings" in 6
	replace label = "FE, wages" in 7

	gen pos_lab = 1

	replace pos_coef = 9 in 6

	#delimit ;
	twoway

	(rcap ci_hi ci_lo column if inrange(num, 6, 7), lwidth(thick) lcolor(gs6) msize(zero))
	(scatter coef column if inrange(num, 6, 7),
		msymbol(circle) msize(4) mfcolor(gs6) mlcolor(gs6)
		mlabel(coef) mlabc(black) mlabv(pos_coef) mlabg(1) mlabsize(3))
	(scatter ci_hi column if inrange(num, 6, 7),
		msymbol(circle) msize(zero) mfcolor(gs6) mlcolor(gs6)
		mlabel(label) mlabc(black) mlabv(pos_lab) mlabg(3) mlabsize(3.1))

	(rcap ci_hi ci_lo column if inrange(num, 4,5) , lwidth(thick) lcolor(gs12) msize(zero))
	(scatter coef column if inrange(num, 4,5),
		msymbol(circle) msize(4) mfcolor(gs12) mlcolor(gs12)
		mlabel(coef) mlabc(black) mlabv(pos_coef) mlabg(1) mlabsize(3))
	(scatter ci_hi column if inrange(num, 4,5),
		msymbol(circle) msize(zero) mfcolor(gs12) mlcolor(gs12)
		mlabel(label) mlabc(black) mlabv(pos_lab) mlabg(3) mlabsize(3.1))

	(rcap ci_hi ci_lo column if num < 3, lwidth(thick) lcolor(gs9) msize(zero))
	(scatter coef column if num <= 3,
		msymbol(circle) msize(4) mfcolor(white) mlcolor(gs12)
		mlabel(coef) mlabc(black) mlabv(pos_coef) mlabg(1) mlabsize(3))
	(scatter ci_hi column if num <= 3,
		msymbol(circle) msize(zero) mfcolor(none) mlcolor(none)
		mlabel(label) mlabc(black) mlabv(pos_lab) mlabg(3) mlabsize(3.1))  ,

	xline(2.9, noextend lcolor(gs8))
	yline(0, lcolor(gs5) lpattern(dash))
	l1title("Productivity Gap (log points)", size(3.6) margin(zero))
	xscale(range(0.5 7) off)
	yscale(range(-0.3 2.1))
	xsize(7.5)
	ysize(5)
	ylabel(0(0.5)2)
	fxsize(136.84)
	graphregion(color(white) margin(vlarge))
	legend(off)
	title("  A. Agriculture/Non-Agriculture, Indonesia", size(3.7) position(11))
	text(2.05 5 "IFLS", place(c) size(3.6))
	text(2.05 1.5 "GLW", place(c) size(3.6))
	name(ind_ag, replace)
	;
	 #delimit cr

	graph save "Figure1_IFLS_agnonag.gph", replace

	restore
