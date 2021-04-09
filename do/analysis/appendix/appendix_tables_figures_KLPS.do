********************************************************************************
*** Appendix Figures and Tables
********************************************************************************
*** Figure A4: Event Study of Urban Migration for Urban Survivors
********************************************************************************
preserve

	clear

	svmat coef_survivor , names(col)
	drop eventime
	svmat coef_notsurvivor, names(col)
	//drop eventime
	//svmat coef, names(col)

	label var eventime "Years relative to first urban move"
	replace eventime = eventime/12


	* Interpolate to get the bounds equal to 1
  foreach point of varlist coef* {
	  sort eventime
		* 1) Identify points that outside of range
		gen oor = !inrange(`point', -1, 1) if !missing(`point')


		* 2) Figure out transition points that switch from out of range to in range
		gen out_in = oor == 1 & oor[_n+1] == 0

		gen Fdata = `point'[_n+1]

		* 3) For those points, generate a new point
		expand 2 if out_in==1, gen(new)
			* Need to make these points missing for other plotted variables
			foreach var of varlist coef* hazard* {
				if "`point'" != "`var'" {
					replace `var' = . if new==1
				}
			}
		sort eventime new

		* 4) Generate plotted points
		* Want to maintain slope. So this will be (y2-y1)/(x2-x1) = (y2-/+1)/(x2-??)
		* x2-?? = (y2-/+1)*(x2-x1)/(y2-y1)
		* ?? = x2 - (y2-/+1)*1/12/(y2-y1)
		replace eventime = eventime + 1/12 - ///
											 (Fdata - sign(Fdata))/12 / ///
											 (Fdata - `point') if new == 1

		replace `point' = sign(Fdata) if new==1


		//list if out_in==1

		drop new out_in Fdata

		* Do the same thing for points that switch from in to out


		* 2) Figure out transition points that switch from out of range to in range
		gen in_out = oor == 1 & oor[_n-1] == 0

		gen Ldata = `point'[_n-1]

		* 3) For those points, generate a new point
		expand 2 if in_out==1, gen(new)
			* Need to make these points missing for other plotted variables
			foreach var of varlist coef* hazard* {
				if "`point'" != "`var'" {
					replace `var' = . if new==1
				}
			}
		sort eventime new

		* 4) Generate plotted points
		* Want to maintain slope. So this will be (y2-y1)/(x2-x1) = (y2-/+1)/(??-x1)
		* ??-x1 = (y2-/+1)*(x2-x1)/(y2-y1)
		* ?? = x1 + (y2-/+1)*1/12/(y2-y1)
		replace eventime = eventime - 1/12 + ///
											 (Ldata - sign(Ldata))/12 / ///
											 (Ldata - `point') if new == 1

		replace `point' = sign(Ldata) if new==1


		//list if in_out==1

		drop new oor in_out Ldata

		* Generate a plot flag
		gen `point'_plotflag = !missing(`point')

		* Last step is to replace all the out of range data with missing
		replace `point' = . if !inrange(`point',-1,1)
	}


	* Graph event study
	local y_pos_survivor = coef_survivor_uci[102]+0.1
	local x_pos_survivor = eventime[102]

	local y_pos_notsurvivor = coef_notsurvivor_lci[108]-0.1
	local x_pos_notsurvivor = eventime[108]

	graph twoway ///
		(line coef_survivor_lci eventime if inrange(coef_survivor_lci,-1,1), lcolor(black) lwidth(thin) lpattern(dash) cmissing(n)) ///
		(line coef_survivor_uci eventime if inrange(coef_survivor_uci,-1,1), lcolor(black) lwidth(thin) lpattern(dash) cmissing(n)) ///
		(line coef_notsurvivor_lci eventime if inrange(coef_notsurvivor_lci,-1,1), lcolor(gs10) lwidth(thin) lpattern(dash) cmissing(n)) ///
		(line coef_notsurvivor_uci eventime if inrange(coef_notsurvivor_uci,-1,1), lcolor(gs10) lwidth(thin) lpattern(dash) cmissing(n)) ///
	  (line coef_survivor eventime if inrange(coef_survivor,-1,1), lcolor(black) lwidth(medthick)) ///
	  (line coef_notsurvivor eventime if inrange(coef_notsurvivor,-1,1), lcolor(gs10) lwidth(medthick)) ///
		(pcarrowi 0.8 -0.3 0.8 -0.8, lcolor(black) mcolor(black) mlabcolor(black)) ///
		(pcarrowi 0.8 0.1 0.8 0.6, lcolor(black) mcolor(black) mlabcolor(black)), /// arrows
		text(0.88 -0.63 "Rural") ///
		text(0.88 0.5 "Urban") ///
		xline(`=-1/12', lcolor(black) lpattern(dot)) ///
		yline(0, lcolor(black) lwidth(thin)) ///
		xlabel(-5(1)5, grid nolabels) ///
		ylabel(-1(0.5)1, nogrid) ///
		graphregion(color(white) lcolor(white)) ///
		legend(off) ///
		xtitle("") ///
		ytitle("Urban Wage Gap (log points)") ///
		text(`y_pos_survivor' `x_pos_survivor'  "Survivor", placement(n) color(black)) ///
		text(`y_pos_notsurvivor' `x_pos_notsurvivor'  "Not Survivor", placement(s) color(gs7)) ///
		name(eventstudy_klps_survivor, replace) ///
		nodraw

	* Graph Survival Rate in Urban areas
	graph twoway ///
		(line hazard_cont_survivor_lci eventime if eventime >= 0 , lwidth(thin) lcolor(black) lpattern(dash) cmissing(y)) ///
		(line hazard_cont_survivor_uci eventime if eventime >= 0 , lwidth(thin) lcolor(black) lpattern(dash) cmissing(y)) ///
	  (line hazard_cont_survivor eventime if eventime >= 0 , lcolor(black) lwidth(medthick)) , ///
		xline(`=-1/12', lcolor(black) lpattern(dot)) ///
		xlabel(-5(1)5, grid) ///
		ylabel(0(20)100, nogrid) ///
		graphregion(color(white) lcolor(white)) ///
		legend(off) ///
		ytitle("Urban Survival Rate (percent)") ///
		name(survival_klps, replace) ///
		nodraw

  graph set eps fontface Garamond
	graph set window fontface Garamond

  graph combine eventstudy_klps_survivor survival_klps , ///
	  rows(2) ///
		imargin(0 0 0 0) ///
		graphregion(color(white) lcolor(white)) ///
		ysize(6)

	graph export "results/figures/appendix/FigureA4_KLPS.eps", replace

	graph set window fontface default
	graph set eps fontface default

restore


********************************************************************************
*** Figure A7: Marginal Distributions of Cognitive Ability
********************************************************************************
use "Intergen_Analysis_KLPS.dta", clear

egen hasmiss = rowmiss(`control5')
drop if hasmiss >0
drop hasmiss

foreach depvar in child_cogn_index {
	eststo `depvar'_reg1: reg `depvar' 1.child_urban_birth, cluster(pupid)
	estadd local square "N"
	eststo `depvar'_reg2: reg `depvar' 1.child_urban_birth child_female, cluster(pupid)
	estadd local square "N"
	eststo `depvar'_reg3: reg `depvar' 1.child_urban_birth child_female parent_female parent_age_atchildbirth, cluster(pupid)
	estadd local square "N"
	eststo `depvar'_reg4: reg `depvar' 1.child_urban_birth child_female parent_female parent_age_atchildbirth parent_educ parent_raven, cluster(pupid)
	estadd local square "N"
	eststo `depvar'_reg5: reg `depvar' 1.child_urban_birth child_female parent_female parent_age_atchildbirth parent_educ parent_raven parent_age_atchildbirth_sq parent_educ_sq parent_raven_sq, cluster(pupid)
	estadd local square "Y"
}

graph twoway ///
  (kdensity child_cogn_index if child_urban_birth==0, lcolor(gs7) lpattern(solid)) ///
	(kdensity child_cogn_index if child_urban_birth==1, lcolor(black) lpatter(dash)), ///
	  graphregion(color(white)) ///
		ytitle("Density") xtitle("Cognitive Ability, Normalized") ///
		legend(label(1 "Born Rural") label(2 "Born Urban")) ///
		xlabel(-4(2)4)
graph export "results/figures/appendix/FigureA7_KLPS.eps", replace


********************************************************************************
*** Figure A8: Joint Distribution of Rural and Urban Productivities
********************************************************************************
use "Main_Analysis_KLPS.dta", clear
* Variable for first observation by pupid
sort pupid yrmth
by pupid: gen obsnum = _n

** Main estimates
gen double pupid_u = pupid*10+urban

areg lninc_h  age_sq i.yrmth [aw=weight], a(pupid_u)
predict pupid_fe, d

* Generate for each person their individual and urban effect
gen temp_r = pupid_fe if urban==0
gen temp_u = pupid_fe if urban==1

egen fe_r = mean(temp_r) , by(pupid)
egen fe_u = mean(temp_u) , by(pupid)

* Normalize to Rural FE for nonmovers
quietly sum fe_r if obsnum==1 & missing(fe_u)
replace fe_r = fe_r - r(mean)
replace fe_u = fe_u - r(mean)

egen fe_overall = mean(pupid_fe) , by(pupid)

gen fe_u_r = fe_u - fe_r

* Labels
label var fe_r "Rural FE"
label var fe_u "Urban FE"
label var fe_u_r "Difference"

* Graph urban FE against rural FE
sum fe_r fe_u if obsnum==1
reg fe_u fe_r if obsnum==1 , robust

local y_pos = _b[_cons] + 4.5*_b[fe_r]
local b = _b[fe_r]
local cons = _b[_cons]

quietly sum fe_r if obsnum==1 & !missing(fe_u)
local min = r(min)
local max = r(max)

graph twoway 	(function y = x, lcolor(gs10) lwidth(medthin) range(-6 6)) ///
	(scatter fe_u fe_r if obsnum==1 & inrange(fe_u, -6,6) & inrange(fe_r,-6,6), msymbol(circle_hollow) mcolor(black) mlwidth(thin)) ///
	(function y = `cons' + `b'*x, color(black) range(-6 6) lpattern(dash) lwidth(medthick))  , ///
graphregion(color(white) lcolor(white)) ///
legend(off) ///
ysca(alt range(-6 6)) ///
xsca(alt range(-6 6)) ///
ylabel(-6(3)6) ///
xlabel(-6(3)6 , grid gmax) ///
ytitle("") ///
xtitle("") ///
text(5 5 "45 degree line", placement(nw)) ///
text(`y_pos' 4.5 "{&beta} = `: di %4.3f `b''", placement(se)) ///
text(`=`y_pos'-0.45' 4.5 "     (`: di %4.3f bstat[2,1]')", placement(se)) ///
name(ur_scatter, replace) ///
nodraw

quietly sum fe_u if obsnum==1 & !missing(fe_r)
graph twoway ///
	(histogram fe_u if obsnum==1 & !missing(fe_r) & inrange(fe_u,-6,6) ,  ///
	start(-5.625) width(0.375) fcolor(none) lcolor(black) lwidth(medthick) horiz ) ///
(scatteri `r(mean)' 0 `r(mean)' 0.6, connect(direct) msymbol(none) lcolor(black) lpattern(dot) lwidth(thick)) , ///
	xsca(alt reverse)  ///
ylabel(-6(3)6, grid gmax) ///
xlabel(minmax, labcolor(white) tlcolor(none)) ///
graphregion(color(white) lcolor(white)) ///
ytitle("Average Urban Productivity") ///
xtitle("") ///
fxsize(25) ///
text(1.2 0.60 "{&mu} = `: di %4.3f r(mean)'", placement(n) orientation(vertical)) ///
text(1.2 0.48 "{&sigma} = `: di %4.3f r(sd)'", placement(n) orientation(vertical)) ///
legend(off) ///
name(ur_hist_u, replace) ///
nodraw


* Rural FE for nonmovers
quietly sum fe_r if obsnum==1 & missing(fe_u)
graph twoway ///
	(histogram fe_r if obsnum==1  & missing(fe_u) & inrange(fe_r,-5.625,6), start(-5.625) width(0.375) fcolor(gs13) lcolor(gs13) lwidth(none)) ///
	(scatteri 0 `r(mean)' 0.6 `r(mean)' , connect(direct) msymbol(none) lcolor(black) lpattern(dot) lwidth(thick)) , ///
ysca(alt) ///
ylabel(0(0.1)0.5 , nogrid labcolor(white) tlcolor(none)) ///
xlabel(-6(3)6, grid gmax) ///
ytitle("") ///
xtitle("Average Rural Productivity") ///
fysize(25) ///
legend(off) ///
graphregion(color(white) lcolor(white)) ///
text(0.63 0.1 "{&mu} = `: di %4.3f 0'", placement(e)) ///
text(0.53 0.1 "{&sigma} = `: di %4.3f r(sd)'", placement(e)) ///
text(0.46 -2.5 "Always Rural", placement(w)) ///
name(ur_hist_r_nonmigrants, replace) ///
nodraw

* Rural FE for Movers
quietly sum fe_r if obsnum==1 & !missing(fe_u)
graph twoway ///
	(histogram fe_r if obsnum==1  & !missing(fe_u) & inrange(fe_r,-5.625,6), start(-5.625) width(0.375) fcolor(white) lcolor(black) lwidth(medthick)) ///
	(scatteri 0 `r(mean)' 0.6 `r(mean)' , connect(direct) msymbol(none) lcolor(black) lpattern(dot) lwidth(thick)) , ///
ysca(alt) ///
ylabel(0(0.1)0.5 , nogrid labcolor(white) tlcolor(none)) ///
xlabel(-6(3)6, grid gmax nolabels) ///
ytitle("") ///
xtitle("") ///
fysize(22) ///
legend(off) ///
graphregion(color(white) lcolor(white)) ///
name(ur_hist_r_migrants, replace) ///
text(0.63 0.6 "{&mu} = `: di %4.3f r(mean)'", placement(e)) ///
text(0.53 0.6 "{&sigma} = `: di %4.3f r(sd)'", placement(e)) ///
text(0.46 -2.5 "Rural-to-Urban Migrants", placement(w)) ///
nodraw

graph set eps fontface Garamond

graph combine ur_hist_u ur_scatter ur_hist_r_migrants ur_hist_r_nonmigrants , ///
	hole(3 5) ///
cols(2) ///
graphregion(color(white) lcolor(white)) ///
xsize(5) ///
ysize(6) ///
imargin(0 0 0 0)

graph export "results/figures/appendix/FigureA8_KLPS_rural.eps" , replace

graph set window fontface default
graph set eps fontface default


********************************************************************************
*** Table A1: Correlates of Employment in Non-Agriculture
********************************************************************************
use "Main_Analysis_KLPS.dta", clear
preserve
egen nonag_ever = max(nonag) , by(pupid)

  drop educpri-educuni

  gen educpri = educlv >= 2 if !missing(educlv)
  gen educsec = educlv >= 6 if !missing(educlv)
  gen educcol = educlv >= 8 if !missing(educlv)

  * 585 kids have years of education = 7 but no education level.

  replace educpri = 0 if missing(educpri) & educyr == 7
  replace educsec = 0 if missing(educsec) & educyr == 7
  replace educcol = 0 if missing(educcol) & educyr == 7

  * To make consistent with years of education measure, only record for
  * kids not missing years of education
  replace educpri = . if missing(educyr)
  replace educsec = . if missing(educyr)
  replace educcol = . if missing(educyr)


  * Collapse the data to the individual level
  collapse (first) ravens educpri educsec educcol ///
	  migrU_ever migr_ever nonag_ever female, by(pupid)


  label var ravens "Raven's Z-score"
  label var educpri "Primary Ed."
  label var educsec "Secondary Ed."
  label var educcol "College"
  label var female "Female"
  label var migrU_ever "Urban Migrants"
  label var migr_ever "Ever Migrated"


  foreach var of varlist educpri educsec educcol female ravens {
    eststo `var': reg nonag_ever `var', robust
  }

  eststo all1: reg nonag_ever educpri educsec educcol female ravens, robust
	eststo all2: reg nonag_ever educpri educsec educcol female, robust

	//blank

  esttab educpri educsec educcol female ravens all1 all2 ///
    using "reuslts/tables/appendix/TableA1_KLPS.tex", replace type fragment ///
	  label ///
		se(3) ///
		nomtitles ///
    title("Correlates of ever being employed in non-agriculture") ///
	  legend ///
	  booktabs ///
	  width(\hsize)


********************************************************************************
*** Table A2: Correlates of Urban Migration
********************************************************************************
  foreach var of varlist educpri educsec educcol female ravens {
    eststo `var': reg migrU_ever `var', robust
  }

  eststo all1: reg migrU_ever educpri educsec educcol female ravens, robust
	eststo all2: reg migrU_ever educpri educsec educcol female, robust

  esttab educpri educsec educcol female ravens all1 all2 ///
    using "reuslts/tables/appendix/TableA2_KLPS.tex", fragment replace type ///
		label ///
		se(3) ///
		nomtitles ///
		title("Correlates of ever migrating to urban") ///
		legend ///
		booktabs ///
		width(\hsize)

restore
********************************************************************************
*** Table A5: Kenya Urban Towns
********************************************************************************
preserve

egen mean_obs = count(pupid) if urban==1, by(town)
quietly sum urban
replace mean_obs = mean_obs/r(sum)*100

egen group = group(mean_obs town) if urban==1, missing

replace group = -group
labmask group, values(town)
label var group town

* Put everything after #20 in a single group
quietly sum group
replace group = r(min)+20 if group >= r(min)+20 | missing(town_pop)
label define group `=r(min)+20' "Other" , modify

gen reported_town_pop = town_pop
replace reported_town_pop = . if group == `=r(min)+20'

* Create the new mean_obs
drop mean_obs
egen mean_obs = count(pupid) if urban==1 , by(group)
quietly sum urban
replace mean_obs = mean_obs/r(sum)*100

label var mean_obs "Percentage of Urban Person-Months"
label var reported_town_pop "Population"

* Also need the populations
bys town urban: gen temp = 1 if _n==1

eststo kenya_urban_town: estpost tabstat mean_obs reported_town_pop if urban==1 & temp==1, by(group) nototal statistic(mean)
esttab kenya_urban_town ///
    using "reuslts/tables/appendix/TableA5_KLPS.tex", fragment replace type ///
		cells("reported_town_pop(label(Population) fmt(%30.0fc)) mean_obs(fmt(1) label(\shortstack{Percentage of Urban\\Individual-Months}))") ///
		noobs ///
		nonumbers ///
		nomtitles ///
		booktabs
restore


********************************************************************************
*** Table A6: Non-Agricultural/Agricultural Gap in Earnings using Alternative Definition of Agriculture
********************************************************************************
*** lninc_nonagonly_KLPS
reg lninc nonagonly [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lninc nonagonly lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg lninc nonagonly lnhour lnhour_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G
xtreg lninc_h nonagonly age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store H

*** lninc_nonagany_KLPS
reg lninc nonagany [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store AA
reg lninc nonagany lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store CC
xtreg lninc nonagany lnhour lnhour_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store GG
xtreg lninc_h nonagany age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store HH
esttab A C G H AA CC GG HH using "reuslts/tables/appendix/TableA6_KLPS.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(c c) collabels(none) /// // alignment(S S)  // collabels("\multicolumn{1}{c}{$\beta$ / SE}" // drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	drop(_cons lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher age_sq *yrmth*) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "" "Log Wage" "" "" "" "Log Wage")  ///
	mgroups("Dependent variable: Log Earnings", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE contr N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Number of observations"' `"Number of individuals"'))


********************************************************************************
*** Table A7: Non-Agricultural/Agricultural Gap in Earnings Within Rural Areas
********************************************************************************
*** If currently in rural area
preserve
keep if urban == 0

*** lninc_nonag_inrural_KLPS
reg lninc nonag [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lninc nonag lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg lninc nonag lnhour lnhour_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G
xtreg lninc_h nonag age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store H

esttab A C G H using "reuslts/tables/appendix/TableA7_KLPS.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(c c) collabels(none) /// // alignment(S S)  // collabels("\multicolumn{1}{c}{$\beta$ / SE}" // drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	drop(_cons *yrmth* voced voced_treat_info voced_treat_voucher lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher age_sq) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "" "Log Wage")  ///
	mgroups("Dependent variable: Log Earnings", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE contr N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Number of observations"' `"Number of individuals"'))
restore


********************************************************************************
*** Table A8: Non-Agricultural/Agricultural Gap in Hours Worked
********************************************************************************
use "Main_Analysis_KLPS.dta", clear

*** lnhour_nonag_KLPS
reg lnhour nonag [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lnhour nonag female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
reg lnhour nonag female age age_sq educyr educyr_sq ravens ravens_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store D
reg lnhour nonag female age age_sq educyr educyr_sq ravens ravens_sq voced voced_treat_info voced_treat_voucher i.yrmth if NA_mover == 1 [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local moverUonly "Y" , replace
estadd local cluster `e(N_clust)', replace
est store E
xtreg lnhour nonag age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G

esttab A C D E G using "reuslts/tables/appendix/TableA8_KLPS.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(c c) collabels(none) /// // alignment(S S)  // collabels("\multicolumn{1}{c}{$\beta$ / SE}" // drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	drop(_cons *yrmth* voced voced_treat_info voced_treat_voucher age age_sq) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "" "" "")  ///
	mgroups("Dependent variable: Log Hours", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE timeFE moverUonly N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Time fixed effects"' `"Switchers only"' `"Number of observations"' `"Number of individuals"'))


********************************************************************************
*** Table A9: Urban/Rural Gap in Hours Worked
********************************************************************************
*** lnhour_urban_KLPS
reg lnhour urban [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lnhour urban female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
reg lnhour urban female age age_sq educyr educyr_sq ravens ravens_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store D
reg lnhour urban female age age_sq educyr educyr_sq ravens ravens_sq voced voced_treat_info voced_treat_voucher i.yrmth if U_mover == 1 [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "N" , replace
estadd local moverUonly "Y" , replace
estadd local cluster `e(N_clust)', replace
est store E
xtreg lnhour urban age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G

esttab A C D E G using "reuslts/tables/appendix/TableA9_KLPS.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(c c) collabels(none) /// // alignment(S S)  // collabels("\multicolumn{1}{c}{$\beta$ / SE}" // drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	drop(_cons *yrmth* voced voced_treat_info voced_treat_voucher age age_sq) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "" "" "")  ///
	mgroups("Dependent variable: Log Hours", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE timeFE moverUonly N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Time fixed effects"' `"Switchers only"' `"Number of observations"' `"Number of individuals"'))


********************************************************************************
*** Table A10: Robustness to Alternative Agricultural Productivity Measures
********************************************************************************
xtreg lninc_h nonag age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local contrhours "N" , replace
estadd local cluster `e(N_clust)', replace
est store A

xtreg lninc_h nonagonly age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local contrhours "N" , replace
estadd local cluster `e(N_clust)', replace
est store B

xtreg lninc_h nonagany age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local contrhours "N" , replace
estadd local cluster `e(N_clust)', replace
est store C

preserve
keep if !missing(lnforinc)
keep if !missing(lnforinc_h)
gen nonagW = nonag
lab var nonagW "Wage employment only"
xtreg lnforinc_h nonagW age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local contrhours "N" , replace
estadd local cluster `e(N_clust)', replace
est store D
restore

preserve
keep if !missing(lninforaginc)
keep if !missing(lninforaginc_h)
gen nonagSA = nonag
lab var nonagSA "Self-employment only"
xtreg lninforaginc_h nonagSA age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local contrhours "N" , replace
estadd local cluster `e(N_clust)', replace
est store E
restore

gen nonagW = nonag
gen nonagSA = nonag
lab var nonagW "Wage employment only"
lab var nonagSA "Self-employment only"

esttab A B C D E, se nostar drop(_cons age_sq *yrmth*)
matrix C = r(coefs)
eststo clear

local rnames: rownames C
matrix b = J(1,5,.)
matrix se = J(1,5,.)
matrix colnames b = `rnames'

forvalues i=1/5 {
  matrix b[1, `i'] = C[`i', `=(`i'-1)*2+1']
	matrix se[1, `i'] = C[`i', `=(`i'-1)*2+2']
}

matrix vcv = diag(se)*diag(se)
matrix colnames vcv = `rnames'
matrix rownames vcv = `rnames'

ereturn clear
ereturn post b vcv
eststo temp

esttab temp using "reuslts/tables/appendix/TableA10_KLPS.tex", replace f type ///
	label booktabs b(3) p(3) eqlabels(none) collabels(none) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	///mlabels("\shortstack{Dependent variable:\\Log Wage}") ///mgroups("Dependent variable: Log Wage", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	nomtitles ///
	order(`rnames') 			///
	cells("b(fmt(3)star)" "se(fmt(3)par)") 	///
	labcol2(" $\checkmark$ \qquad\qquad\qquad\qquad $\checkmark$" ///
	        " $\checkmark$ \qquad\qquad\qquad\qquad $\checkmark$" ///
					" $\checkmark$ \qquad\qquad\qquad\qquad $\checkmark$" ///
					" $\checkmark$ \qquad\qquad\qquad\qquad \phantom{$\checkmark$}" ///
					" \phantom{$\checkmark$} \qquad\qquad\qquad\qquad $\checkmark$") ///
					///title("\shortstack[l]{Productivity Measure Includes...\\Formal inforinc_h \quad Self-Employed Profits}")) ///
	nonumbers noobs ///
  prehead("Definition of Agriculture&\shortstack[l]{Productivity Measure Includes...\\Formal inforinc_h \quad Self-Employed Profits}&\shortstack{Dependent variable:\\Log Wage}\\") ///
	varlabel(nonag "\textbf{\shortstack[l]{Majority of hours in agriculture\\Main Estimation}}" ///
	         nonagonly "Any hours in agriculture" ///
					 nonagany "All hours in agriculture" ///
					 nonagW "Majority of hours in agriculture" ///
					 nonagSA "Majority of hours in agriculture")

gen ag_forinc = 1 if !missing(forinc) & forinc > 0 & nonag==0
gen ag_self1 = 1 if !missing(inforinc) & inforinc > 0 & nonag==0
gen ag_ag1 = 1 if !missing(aginc) & aginc > 0 & nonag==0

bys ag_forinc pupid: gen first_forag = 1 if _n==1 & ag_forinc==1
bys ag_self1 pupid: gen first_self1 = 1 if _n==1 & ag_self1==1
bys ag_ag1 pupid: gen first_ag1 = 1 if _n==1 & ag_ag1==1

count if ag_forinc==1
count if first_forag==1

count if ag_self1==1
count if first_self1==1

count if ag_ag1==1
count if first_ag1==1

drop ag_forinc ag_self1 ag_ag1 first_forag first_self1 first_ag1


********************************************************************************
*** Table A12: Gap in Wage Earnings
********************************************************************************
preserve
keep if !missing(lnforinc)
keep if !missing(lnforinc_h)

*** lnforinc_nonag_KLPS
reg lnforinc nonag [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lnforinc nonag lnforhour lnforhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg lnforinc nonag lnforhour lnforhour_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G
xtreg lnforinc_h nonag age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store H

*** lnforinc_urban_KLPS
reg lnforinc urban [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store AA
reg lnforinc urban lnforhour lnforhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store CC
xtreg lnforinc urban lnforhour lnforhour_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store GG
xtreg lnforinc_h urban age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store HH
esttab A C G H AA CC GG HH using "reuslts/tables/appendix/TableA12_KLPS.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(c c) collabels(none) /// // alignment(S S)  // collabels("\multicolumn{1}{c}{$\beta$ / SE}" // drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	drop(_cons lnforhour lnforhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher age_sq *yrmth*) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "" "Log Wage" "" "" "" "Log Wage")  ///
	mgroups("Dependent variable: Log Wage Earnings", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE contr N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Number of observations"' `"Number of individuals"'))

restore


********************************************************************************
*** Table A13: Gap in Self-Employment Earnings
********************************************************************************
preserve
keep if !missing(lninforaginc)
keep if !missing(lninforaginc_h)

*** lninforinc_nonag_KLPS
reg lninforaginc nonag [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lninforaginc nonag lninforaghour lnhourSA_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg lninforaginc nonag lninforaghour lnhourSA_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G
xtreg lninforaginc_h nonag age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store H

*** lninforinc_urban_KLPS
reg lninforaginc urban [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store AA
reg lninforaginc urban lninforaghour lnhourSA_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store CC
xtreg lninforaginc urban lninforaghour lnhourSA_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store GG
xtreg lninforaginc_h urban age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store HH
esttab A C G H AA CC GG HH using "reuslts/tables/appendix/TableA13_KLPS.tex", replace f type ///
	label booktabs b(3) p(3) eqlabels(none) alignment(c c) collabels(none) /// // alignment(S S)  // collabels("\multicolumn{1}{c}{$\beta$ / SE}" // drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	keep(nonag urban) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "" "Log Wage" "" "" "" "Log Wage")  ///
	mgroups("Dependent variable: Log Self-Employment Earnings", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE contr N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Number of observations"' `"Number of individuals"'))

restore


********************************************************************************
*** Table A14: Alternative Samples Kenya
********************************************************************************
preserve

gen lnhour_allhours_sq = lnhour_allhours * lnhour_allhours

drop nonag
gen nonag = .
replace nonag = 1 if nonaghour > aghour_allhours & aghour_allhours ~= . & nonaghour ~= .
replace nonag = 0 if nonaghour < aghour_allhours & aghour_allhours ~= . & nonaghour ~= .
replace nonag = 1 if nonaghour > 0 & nonaghour ~= . & aghour_allhours == .
replace nonag = 0 if aghour_allhours > 0 & nonaghour == . & aghour_allhours ~= .

replace nonag = 0 if (abs(aghour_allhours - nonaghour) < 10^-2) & aghour_allhours ~= . & nonaghour ~= .
replace nonag = . if nonaghour == 0 & aghour_allhours == 0
replace nonag = . if nonaghour == 0 & aghour_allhours == .
replace nonag = . if nonaghour == . & aghour_allhours == 0
lab var nonag "Non-agricultural employment"

*** lnforinforinc_KLPS_allhours
reg lninc_allhours nonag [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lninc_allhours nonag lnhour_allhours lnhour_allhours_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg lninc_allhours nonag lnhour_allhours lnhour_allhours_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store F
xtreg lninc_h_allhours nonag age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G

*** lnforinforinc_KLPS_allhours
reg lninc_allhours urban [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store AA
reg lninc_allhours urban lnhour_allhours lnhour_allhours_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store CC
xtreg lninc_allhours urban lnhour_allhours lnhour_allhours_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store FF
xtreg lninc_h_allhours urban age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store GG
esttab A C F G AA CC FF GG using "reuslts/tables/appendix/TableA14_KLPS_PanelA.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(c c) collabels(none) /// // alignment(S S)  // collabels("\multicolumn{1}{c}{$\beta$ / SE}" // drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	drop(_cons lnhour_allhours lnhour_allhours_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher age_sq *yrmth*) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "" "Log Wage" "" "" "" "Log Wage")  ///
	mgroups("Dependent variable: Log Wage Earnings", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE contr N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Number of observations"' `"Number of individuals"'))

restore

preserve

gen lnforinforhour_sq = lnforinforhour * lnforinforhour

drop nonag
gen nonag = .
replace nonag = 1 if nonaghour > aghour & aghour ~= . & nonaghour ~= .
replace nonag = 0 if nonaghour < aghour & aghour ~= . & nonaghour ~= .
replace nonag = 1 if nonaghour > 0 & nonaghour ~= . & aghour == .
replace nonag = 0 if aghour > 0 & nonaghour == . & aghour ~= .

replace nonag = 0 if (abs(aghour - nonaghour) < 10^-2) & aghour ~= . & nonaghour ~= .
replace nonag = . if nonaghour == 0 & aghour == 0
replace nonag = . if nonaghour == 0 & aghour == .
replace nonag = . if nonaghour == . & aghour == 0
lab var nonag "Non-agricultural employment"

*** lnforinforinc_KLPS_NoAg
reg lnforinforinc nonag [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lnforinforinc nonag lnforinforhour lnforinforhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg lnforinforinc nonag lnforinforhour lnforinforhour_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store F
xtreg lnforinforinc_h nonag age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store G

*** lnforinforinc_KLPS_NoAg
reg lnforinforinc urban [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store AA
reg lnforinforinc urban lnforinforhour lnforinforhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store CC
xtreg lnforinforinc urban lnforinforhour lnforinforhour_sq age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store FF
xtreg lnforinforinc_h urban age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store GG
esttab A C F G AA CC FF GG using "reuslts/tables/appendix/TableA14_KLPS_PanelB.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(c c) collabels(none) /// // alignment(S S)  // collabels("\multicolumn{1}{c}{$\beta$ / SE}" // drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	drop(_cons lnforinforhour lnforinforhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher age_sq *yrmth*) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "" "Log Wage" "" "" "" "Log Wage")  ///
	mgroups("Dependent variable: Log Wage Earnings", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE contr N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Number of observations"' `"Number of individuals"'))

restore


********************************************************************************
*** Table A15: Unemployment and Job Search Behavior, Kenya
********************************************************************************
use "Consumption_Analysis_KLPS.dta", clear

keep if !missing(urban)
keep if !missing(female)
keep if !missing(age)
keep if !missing(educyr)
keep if sample_unempl == 1
preserve
*** unempl_or_ag_urban_KLPS
reg unempl_or_ag urban [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local depmean `: di %05.3f `unempl_or_ag_mean'', replace
estadd local cluster `e(N_clust)', replace
est store A
reg unempl_or_ag urban female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local depmean `: di %05.3f `unempl_or_ag_mean'', replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg unempl_or_ag urban age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local depmean `: di %05.3f `unempl_or_ag_mean'', replace
estadd local cluster `e(N_clust)', replace
est store F
reg unempl urban [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local depmean `: di %05.3f `unempl_mean'', replace
estadd local cluster `e(N_clust)', replace
est store AA
reg unempl urban female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local depmean `: di %05.3f `unempl_mean'', replace
estadd local cluster `e(N_clust)', replace
est store CC
xtreg unempl urban age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local depmean `: di %05.3f `unempl_mean'', replace
estadd local cluster `e(N_clust)', replace
est store FF
esttab A C F AA CC FF using "reuslts/tables/appendix/TableA15_unemp.tex", fragment replace type ///
  label booktabs b(3) p(3) eqlabels(none) collabels(none) ///
	drop(_cons female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher age_sq *yrmth*) ///
	star(* 0.10 ** 0.05 *** 0.01) /// // mtitle(("test" "" "" "hello" "" "") pattern(1 0 0 1 0 0))  /// // "Movers" "Urban Movers"
	mgroups("\shortstack{Dependent Variable: Unemployment\\or Subsistence Agriculture}" "\shortstack{Dependent Variable: Unemployment\\ \ }", pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	nomtitles ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///	//	stats(N, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"'))
	stats(indFE contr depmean N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Mean dependent variable"' `"Number of observations"' `"Number of individuals"'))

*** searchtot_urban_KLPS
reg searchtot urban [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local depmean `: di %05.3f `searchtot_mean'', replace
estadd local cluster `e(N_clust)', replace
est store A
reg searchtot urban female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local depmean `: di %05.3f `searchtot_mean'', replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg searchtot urban age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local depmean `: di %05.3f `searchtot_mean'', replace
estadd local cluster `e(N_clust)', replace
est store F
esttab A C F using "reuslts/tables/appendix/TableA15_search.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(S S) collabels(none) ///	// drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	drop(_cons female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher age_sq *yrmth*) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	nomtitles	/// // 	mtitle("" "" "")  /// // "Movers" "Urban Movers"
	mgroups("Dependent variable: Total Hours Job Search", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///	//	stats(N, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"'))
	stats(indFE contr depmean N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Mean dependent variable"' `"Number of observations"' `"Number of individuals"'))

restore


********************************************************************************
*** Table A16: Alternative Coefficient Standard Error Estimation
********************************************************************************
run "6. Cr23_ik_css_mod.do"
run "7. Edfreg_mod.do"
local bsreps = 1000

xtset, clear
local urban urban

* Recode the time FE
tab yrmth, gen(yrmth_)
drop yrmth_1
global timeFE yrmth_*


* Residualize some variables for Frisch-Waugh to accommodate CR2 and CR3 SE
foreach var of varlist lninc lninc_h `urban' nonag {
  xtreg `var', fe i(pupid)
	predict res_`var', e
}

* Residualize all the covariates with respect to FE
local residualized1
local residualized2
foreach var of varlist age_sq $timeFE {
  xtreg `var', fe i(pupid)
	predict res_`var', e
	local residualized1 `residualized1' res_`var'
}

local residualized2 `residualized1'
foreach var of varlist lnhour lnhour_sq {
  xtreg `var', fe i(pupid)
	predict res_`var', e
	local residualized2 `residualized2' res_`var'
}



reg lninc `urban', cluster(pupid)
est store A
estadd local contr "N" , replace: A
estadd local indFE "N" , replace: A
estadd local cluster `e(N_clust)', replace: A

* Regular clustering
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_default = vecdiag(temp)

estadd matrix stderr_default: A

** CR2 and 3 DOF corrections

predict resid, resid
matrix betavec = e(b)

CR23_IK_CSS , betavec(betavec) lhs(lninc) rhs(`urban') key_rhs(`urban') cluster(pupid)

matrix stderr_cr2 = stderr_default
matrix stderr_cr2[1, 1] = r(se_CR2)
estadd matrix stderr_cr2: A

matrix stderr_cr3 = stderr_default
matrix stderr_cr3[1, 1] = r(se_CR3)
estadd matrix stderr_cr3: A

drop resid

reg lninc `urban', vce(bootstrap, cluster(pupid) reps(`bsreps'))

matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_bootstrap = vecdiag(temp)

estadd matrix stderr_bootstrap: A

edfreg lninc `urban', cluster(pupid)  select(`urban')

matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_young = vecdiag(temp)

estadd matrix stderr_young: A

reg lninc `urban' lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth, cluster(pupid)
est store C
estadd local contr "Y" , replace: C
estadd local indFE "N" , replace: C
estadd local cluster `e(N_clust)', replace: C

* Regular clustering
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_default = vecdiag(temp)

estadd matrix stderr_default: C

** CR2 and 3 DOF corrections

predict resid, resid
matrix betavec = e(b)

CR23_IK_CSS , betavec(betavec) lhs(lninc) rhs(`urban' lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher $timeFE) key_rhs(`urban') cluster(pupid)


matrix stderr_cr2 = stderr_default
matrix stderr_cr2[1, 1] = r(se_CR2)
estadd matrix stderr_cr2: C

matrix stderr_cr3 = stderr_default
matrix stderr_cr3[1, 1] = r(se_CR3)
estadd matrix stderr_cr3: C

drop resid

areg lninc `urban' lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher, vce(bootstrap, cluster(pupid) reps(`bsreps')) a(yrmth)
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_bootstrap = vecdiag(temp)

estadd matrix stderr_bootstrap: C

edfreg lninc `urban' lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher $timeFE, cluster(pupid) select(`urban')
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_young = vecdiag(temp)

estadd matrix stderr_young: C

xtreg lninc `urban' lnhour lnhour_sq age_sq i.yrmth, fe i(pupid) cluster(pupid)			// male educ_std
est store F
estadd local contr "Y" , replace: F
estadd local indFE "Y" , replace: F
estadd local cluster `e(N_clust)', replace: F

* Regular clustering
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_default = vecdiag(temp)

estadd matrix stderr_default: F

** CR2 and 3 DOF corrections
reg res_lninc res_`urban' `residualized2', cluster(pupid) noconstant
predict resid, resid
matrix betavec = e(b)

CR23_IK_CSS , betavec(betavec) lhs(res_lninc) rhs(" res_`urban' `residualized2'") key_rhs(res_`urban') cluster(pupid) noconstant(1)

matrix stderr_cr2 = stderr_default
matrix stderr_cr2[1, 1] = r(se_CR2)
estadd matrix stderr_cr2: F

matrix stderr_cr3 = stderr_default
matrix stderr_cr3[1, 1] = r(se_CR3)
estadd matrix stderr_cr3: F

drop resid

mata: findexternal("cr2cr3()")
bootstrap _b[`urban'], cluster(pupid) reps(`bsreps'): reghdfe lninc `urban' lnhour lnhour_sq age_sq, a(pupid yrmth)
mata: findexternal("cr2cr3()")
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_bootstrap = vecdiag(temp)
matrix colnames stderr_bootstrap = `urban'
matrix rownames stderr_bootstrap = `urban'

estadd matrix stderr_bootstrap: F

mata: findexternal("cr2cr3()")
mata: mata set matastrict off
run "6. Cr23_ik_css_mod.do"

edfreg lninc `urban' lnhour lnhour_sq age_sq $timeFE, cluster(pupid) absorb(pupid) select(`urban')
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_young = vecdiag(temp)

estadd matrix stderr_young: F

xtreg lninc_h `urban' age_sq i.yrmth, fe i(pupid) cluster(pupid)
est store G
estadd local contr "Y" , replace: G
estadd local indFE "Y" , replace: G
estadd local cluster `e(N_clust)', replace: G

* Regular clustering
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_default = vecdiag(temp)

estadd matrix stderr_default: G

** CR2 and 3 DOF corrections
reg res_lninc_h res_`urban' `residualized1', cluster(pupid) noconstant
predict resid, resid
matrix betavec = e(b)
CR23_IK_CSS, betavec(betavec) lhs(res_lninc_h) rhs("res_`urban' `residualized1'") key_rhs(res_`urban') cluster(pupid) noconstant(1)

matrix stderr_cr2 = stderr_default
matrix stderr_cr2[1, 1] = r(se_CR2)
estadd matrix stderr_cr2: G

matrix stderr_cr3 = stderr_default
matrix stderr_cr3[1, 1] = r(se_CR3)
estadd matrix stderr_cr3: G

drop resid

bootstrap _b[`urban'], cluster(pupid) reps(`bsreps'): reghdfe lninc_h `urban' age_sq, a(yrmth pupid)
mata: findexternal("cr2cr3()")
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_bootstrap = vecdiag(temp)
matrix colnames stderr_bootstrap = `urban'
matrix rownames stderr_bootstrap = `urban'

estadd matrix stderr_bootstrap: G

edfreg lninc_h `urban' age_sq $timeFE, cluster(pupid) absorb(pupid) select(`urban')
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_young = vecdiag(temp)

estadd matrix stderr_young: G

reg lninc nonag, cluster(pupid)
est store AA
estadd local contr "N" , replace: AA
estadd local indFE "N" , replace: AA
estadd local cluster `e(N_clust)', replace: AA

* Regular clustering
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_default = vecdiag(temp)

estadd matrix stderr_default: AA

** CR2 and 3 DOF corrections

predict resid, resid
matrix betavec = e(b)

CR23_IK_CSS , betavec(betavec) lhs(lninc) rhs(nonag) key_rhs(nonag) cluster(pupid)


matrix stderr_cr2 = stderr_default
matrix stderr_cr2[1, 1] = r(se_CR2)
estadd matrix stderr_cr2: AA

matrix stderr_cr3 = stderr_default
matrix stderr_cr3[1, 1] = r(se_CR3)
estadd matrix stderr_cr3: AA

drop resid

reg lninc nonag, vce(bootstrap, cluster(pupid) reps(`bsreps'))
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_bootstrap = vecdiag(temp)

estadd matrix stderr_bootstrap: AA

edfreg lninc nonag, cluster(pupid) select(nonag)
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_young = vecdiag(temp)

estadd matrix stderr_young: AA

reg lninc nonag lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth, cluster(pupid)
est store CC
estadd local contr "Y" , replace: CC
estadd local indFE "N" , replace: CC
estadd local cluster `e(N_clust)', replace: CC

* Regular clustering
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_default = vecdiag(temp)

estadd matrix stderr_default: CC

** CR2 and 3 DOF corrections

predict resid, resid
matrix betavec = e(b)

CR23_IK_CSS , betavec(betavec) lhs(lninc) rhs(nonag lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher $timeFE) key_rhs(nonag) cluster(pupid)


matrix stderr_cr2 = stderr_default
matrix stderr_cr2[1, 1] = r(se_CR2)
estadd matrix stderr_cr2: CC

matrix stderr_cr3 = stderr_default
matrix stderr_cr3[1, 1] = r(se_CR3)
estadd matrix stderr_cr3: CC

drop resid

areg lninc nonag lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher, vce(bootstrap, cluster(pupid) reps(`bsreps')) a(yrmth)
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_bootstrap = vecdiag(temp)

estadd matrix stderr_bootstrap: CC

edfreg lninc nonag lnhour lnhour_sq female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher $timeFE, cluster(pupid) select(nonag)
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_young = vecdiag(temp)

estadd matrix stderr_young: CC

xtreg lninc nonag lnhour lnhour_sq age_sq i.yrmth, fe i(pupid) cluster(pupid)			// male educ_std
est store FF
estadd local contr "Y" , replace: FF
estadd local indFE "Y" , replace: FF
estadd local cluster `e(N_clust)', replace: FF

* Regular clustering
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_default = vecdiag(temp)

estadd matrix stderr_default: FF

** CR2 and 3 DOF corrections
reg res_lninc res_nonag `residualized2', cluster(pupid) noconstant
predict resid, resid
matrix betavec = e(b)

CR23_IK_CSS , betavec(betavec) lhs(res_lninc) rhs("res_nonag `residualized2'") key_rhs(res_nonag) cluster(pupid) noconstant(1)


matrix stderr_cr2 = stderr_default
matrix stderr_cr2[1, 1] = r(se_CR2)
estadd matrix stderr_cr2: FF

matrix stderr_cr3 = stderr_default
matrix stderr_cr3[1, 1] = r(se_CR3)
estadd matrix stderr_cr3: FF

drop resid

mata: findexternal("cr2cr3()")
bootstrap _b[nonag], cluster(pupid) reps(`bsreps'): reghdfe lninc nonag lnhour lnhour_sq age_sq, a(pupid yrmth)
mata: findexternal("cr2cr3()")
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_bootstrap = vecdiag(temp)
matrix colnames stderr_bootstrap = nonag
matrix rownames stderr_bootstrap = nonag

estadd matrix stderr_bootstrap: FF

* It does it here too
mata: findexternal("cr2cr3()")
mata: mata set matastrict off
run "6. Cr23_ik_css_mod.do"

edfreg lninc nonag lnhour lnhour_sq age_sq $timeFE, cluster(pupid) absorb(pupid) select(nonag)
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_young = vecdiag(temp)

estadd matrix stderr_young: FF

xtreg lninc_h nonag age_sq i.yrmth, fe i(pupid) cluster(pupid)
est store GG
estadd local contr "Y" , replace: GG
estadd local indFE "Y" , replace: GG
estadd local cluster `e(N_clust)', replace: GG

* Regular clustering
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_default = vecdiag(temp)

estadd matrix stderr_default: GG

** CR2 and 3 DOF corrections
reg res_lninc_h res_nonag `residualized1', cluster(pupid) noconstant
predict resid, resid
matrix betavec = e(b)

CR23_IK_CSS , betavec(betavec) lhs(res_lninc_h) rhs("res_nonag `residualized1'") key_rhs(res_nonag) cluster(pupid) noconstant(1)


matrix stderr_cr2 = stderr_default
matrix stderr_cr2[1, 1] = r(se_CR2)
estadd matrix stderr_cr2: GG

matrix stderr_cr3 = stderr_default
matrix stderr_cr3[1, 1] = r(se_CR3)
estadd matrix stderr_cr3: GG

drop resid


bootstrap _b[nonag], cluster(pupid) reps(`bsreps'): reghdfe lninc_h nonag age_sq, a(pupid yrmth)
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_bootstrap = vecdiag(temp)
matrix colnames stderr_bootstrap = nonag
matrix rownames stderr_bootstrap = nonag

estadd matrix stderr_bootstrap: GG

edfreg lninc_h nonag age_sq $timeFE, cluster(pupid) absorb(pupid) select(nonag)
matrix diagonal = diag(vecdiag(e(V)))
matmap diagonal temp, map(sqrt(@))
matrix stderr_young = vecdiag(temp)

estadd matrix stderr_young: GG

esttab AA CC FF GG A C F G using "reuslts/tables/appendix/TableA16_KLPS.tex", replace type f ///
  cells(b(fmt(3)star) stderr_default(par(( ))) stderr_cr2(par([ ])) stderr_cr3(par(`"\$\\llbracket\$"' `"\$\\rrbracket\$"')) stderr_bootstrap(par(\{ \})) stderr_young(par(`"\$\\langle"' `"\\rangle\$"'))) ///
	keep(urban nonag) ///
	label booktabs b(3) p(3) eqlabels(none) alignment(S S) collabels(none) ///	// drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("" "" "" "Log Wage" "" "" "" "Log Wage")  /// // "Movers" "Urban Movers"
	mgroups("Dependent variable: Log Earnings (in IDR)", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	stats(indFE contr N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Number of observations"' `"Number of individuals"'))


drop res_lninc res_lninc_h res_`urban' res_nonag `residualized2'


********************************************************************************
*** Table A17: Integenerational Correlations of Cognitive Measures
********************************************************************************
use "Intergen_Analysis_KLPS.dta", clear

local control2 child_female
local control3 child_female parent_female parent_age_atchildbirth
local control4 `control3' parent_educ parent_raven
local control5 `control4' parent_age_atchildbirth_sq parent_educ_sq parent_raven_sq

egen hasmiss = rowmiss(`control5')
drop if hasmiss >0
drop hasmiss

foreach depvar in child_cogn_index {
	eststo `depvar'_reg1: reg `depvar' 1.child_urban_birth, cluster(pupid)
	estadd local square "N"
	eststo `depvar'_reg2: reg `depvar' 1.child_urban_birth `control2', cluster(pupid)
	estadd local square "N"
	eststo `depvar'_reg3: reg `depvar' 1.child_urban_birth `control3', cluster(pupid)
	estadd local square "N"
	eststo `depvar'_reg4: reg `depvar' 1.child_urban_birth `control4', cluster(pupid)
	estadd local square "N"
	eststo `depvar'_reg5: reg `depvar' 1.child_urban_birth `control5', cluster(pupid)
	estadd local square "Y"
}

esttab child_cogn_index_reg* using "reuslts/tables/appendix/TableA17_KLPS.tex", ///
	booktabs type replace fragment ///
  b(%5.3f) se(%5.3f) ///
  star(* 0.10 ** 0.05 *** 0.01) ///
	mgroup("\shortstack{Dependent variable: Normalized Cognitive Ability Index}", ///
	         pattern(1 0 0 0 0) span prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
	mlabels(none) label interaction("$\times$") ///
	drop(_cons *_sq) ///
	varlabels( ///
	  child_female Female ///
	  parent_female Female ///
		parent_age_atchildbirth "Age at Birth" ///
		parent_educ "Years of Education" ///
		parent_raven "Normalized Ravens") ///
	refcat(1.child_urban_birth "\textbf{Child Covariates:}" ///
	       parent_female "\textbf{KLPS Parent Covariates:}", nolabel) ///
	stats(square N, ///
		labels("Age, Education, and Ravens Squared" ///
		       "Number of observations") ///
		fmt(%9.0g) ///
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")) ///
	nonotes


********************************************************************************
*** Table A18: Correlates of Meals Eaten—Kenya
********************************************************************************
use "Consumption_Analysis_KLPS.dta", clear

foreach var of varlist lncons_tot lninc lninc_h {
  eststo `var': reg `var' logmeals , cluster(pupid)
}

esttab lncons_tot lninc lninc_h ///
	using "reuslts/tables/appendix/TableA18_KLPS.tex", f replace type ///
	label ///
	mtitle("Log Consumption" "Log Earnings" "Log Wage")  ///
	drop(_cons) ///
	se(3) ///
	legend ///
	stats(N, fmt(0) layout("\multicolumn{1}{c}{@}") labels("Number of observations")) ///
	booktabs


********************************************************************************
*** Table A19: Gaps in Consumption
********************************************************************************
use "Meals_Analysis_KLPS.dta", clear
xtset pupid yrmth

* generate logs of meals

*** lncons_food_IFLS
reg logmeals nonag [aw=weight], cluster(pupid)
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store mealsgap_nonag_raw

reg logmeals nonag female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid)						// lnhour lnhour_sq
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store mealsgap_nonag_ctrl

xtreg logmeals nonag age_sq i.yrmth, fe i(pupid) cluster(pupid)	// lnhour lnhour_sq
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store mealsgap_nonag_fe

reg logmeals urban [aw=weight], cluster(pupid)
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store mealsgap_urban_raw

reg logmeals urban female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid)						// lnhour lnhour_sq
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store mealsgap_urban_ctrl

xtreg logmeals urban age_sq i.yrmth, fe i(pupid) cluster(pupid)	// lnhour lnhour_sq
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store mealsgap_urban_fe

esttab ///
  mealsgap_nonag_raw ///
  mealsgap_nonag_ctrl ///
  mealsgap_nonag_fe ///
	mealsgap_urban_raw ///
  mealsgap_urban_ctrl ///
	mealsgap_urban_fe ///
  using "reuslts/tables/appendix/TableA19_KLPS.tex", replace f type ///
	label booktabs b(3) p(3) eqlabels(none) alignment(S S) collabels(none)	///	// indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	drop(_cons female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher age_sq *yrmth*) ///	// female age age_sq educyr educyr_sq ravens ravens_sq voced voced_treat_info voced_treat_voucher
	star(* 0.10 ** 0.05 *** 0.01) /// //	collabels("\multicolumn{3}{c}{test}" "\multicolumn{3}{c}{test2}") ///
	nomtitles /// // mtitle("" "" "" "" "" "")  /// //
	mgroups("Dependent variable: Log Meals Eaten", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	order(nonag urban) 			///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///	//	stats(N, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"'))
	stats(indFE contr N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Number of observations"' `"Number of individuals"'))


********************************************************************************
*** Table A23: Urban/Rural Gap in inforinc_h for Top 5 Cities
********************************************************************************
use "Main_Analysis_KLPS.dta", clear

reg lninc_h urban [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A0
reg lninc_h urban nairobi mombasa kisumu nakuru eldoret [aw=weight], cluster(pupid) robust
estadd local timeFE "N" , replace
estadd local contr "N" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store A
reg lninc_h urban nairobi mombasa kisumu nakuru eldoret female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher i.yrmth [aw=weight], cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "N" , replace
estadd local cluster `e(N_clust)', replace
est store C
xtreg lninc_h urban nairobi mombasa kisumu nakuru eldoret age_sq i.yrmth [aw=weight], fe i(pupid) cluster(pupid) robust
estadd local timeFE "Y" , replace
estadd local contr "Y" , replace
estadd local indFE "Y" , replace
estadd local cluster `e(N_clust)', replace
est store F
esttab A0 A C F using "reuslts/tables/appendix/TableA23_KLPS.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(c c) collabels(none) /// // alignment(S S)  // collabels("\multicolumn{1}{c}{$\beta$ / SE}" // drop(yr1998mth1 yr1998mth2) ///	//	indicate("Time FE =i.yrmth" "Individual FE = $test", labels(Y N)) ///
	drop(_cons female age age_sq educyr educyr_sq voced voced_treat_info voced_treat_voucher age_sq *yrmth*) ///
	star(* 0.10 ** 0.05 *** 0.01) ///	//
	nomtitles	/// // mtitle("" "" "" "")  ///
	mgroups("Dependent variable: Log inforinc_h", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") ///
	stats(indFE contr N cluster, fmt(0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Individual fixed effects"' `"Control variables and time FE"' `"Number of observations"' `"Number of individuals"'))
