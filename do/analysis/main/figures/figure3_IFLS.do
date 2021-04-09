********************************************************************************
*** Figure 3: Event Study of Urban Migration
********************************************************************************
	preserve

	sort pidlink year
	by pidlink: gen obsnum = _n

	gen moveR2U = 1 if urban == 1 & l.urban == 0 & urban_birth == 0
	gen temp = year if moveR2U == 1
	egen firstmoveR2U = min(temp) , by(pidlink)
	drop temp

gen eventime = year - firstmoveR2U

	forvalues i = -5/5 {
		if `i' < 0 {
			gen timedum_`=-`i'' = eventime==`i'
		}
		else {
			gen timedum`i' = eventime==`i'
		}
	}

	gen endpoint_pre = eventime <= -6
	gen endpoint_post = eventime >= 6
	replace endpoint_post = 0 if missing(eventime)

	gen temp = eventime if urban==0 & eventime > 0
	egen first_postrural = min(temp) , by(pidlink)
	drop temp

	gen stillurban = eventime < first_postrural & eventime >= 0

	gen temp = jakarta if eventime==0
	egen firsturban_jakarta = mean(temp), by(pidlink)
	drop temp

	forvalues i=-5/5 {
		if `i' < 0 {
			gen timedum_jakarta_`=-`i'' = eventime==`i'&(firsturban_jakarta==1)
			gen timedum_notjakarta_`=-`i'' = eventime==`i'&(firsturban_jakarta==0)
		}
		else {
			gen timedum_jakarta`i' = eventime==`i'&(firsturban_jakarta==1)
			gen timedum_notjakarta`i' = eventime==`i'&(firsturban_jakarta==0)
		}
	}

	gen endpoint_jakarta_pre = endpoint_pre & (firsturban_jakarta==1)
	gen endpoint_notjakarta_pre = endpoint_pre & (firsturban_jakarta==0)

	gen endpoint_jakarta_post = endpoint_post & (firsturban_jakarta==1)
	gen endpoint_notjakarta_post = endpoint_post & (firsturban_jakarta==0)

	gen stillurban_jakarta = stillurban if firsturban_jakarta==1
	gen stillurban_notjakarta = stillurban if firsturban_jakarta==0

	forvalues i=1/5 {
			gen timedum_survivor`i' = eventime==`i' & (stillurban==1)
			gen timedum_notsurvivor`i' = eventime==`i' & (stillurban==0)
  }

	gen endpoint_survivor_post = endpoint_post & (stillurban==1)
	gen endpoint_notsurvivor_post = endpoint_post & (stillurban==0)

	order timedum_notjakarta* , after(endpoint_jakarta_post)
	order endpoint_notjakarta* , after(timedum_notjakarta5)
	order stillurban_*jakarta , after(endpoint_notjakarta_post)
	order timedum_survivor*, after(stillurban_notjakarta)
	order timedum_notsurvivor*, after(timedum_survivor5)
	order endpoint_survivor*, after(timedum_notsurvivor5)

  xtreg lninc_h timedum_5-timedum_2 /// pre
	              timedum0-timedum5 /// post
								endpoint_pre endpoint_post /// pooled endpoints
								age_sq i.year , ///
	  fe i(pidlink) cluster(pidlink) robust // controls

	count if timedum0 == 1
	count if timedum5 == 1

	matrix coef = J(11,10,.)
	matrix colnames coef = eventime /// 1
	  coef coef_lci coef_uci /// 4
	  frac_urban frac_urban_lci frac_urban_uci /// 7
		hazard_cont hazard_cont_lci hazard_cont_uci
	forvalues i=-5/5 {
		matrix coef[`i'+6,1] = `i'

		if `i' <= -2 {

		  matrix coef[`i'+6,2] = _b[timedum_`=-`i'']
			matrix coef[`i'+6,3] = _b[timedum_`=-`i''] - _se[timedum_`=-`i'']*invttail(e(df_r), 0.025)
			matrix coef[`i'+6,4] = _b[timedum_`=-`i''] + _se[timedum_`=-`i'']*invttail(e(df_r), 0.025)

			matrix coef[`i'+6,5] = 0
			matrix coef[`i'+6,8] = 0
		}
		if `i' == -1 {

		  matrix coef[`i'+6,2] = 0

			matrix coef[`i'+6,5] = 0
			matrix coef[`i'+6,8] = 0
		}
		if `i' >= 0 {

			matrix coef[`i'+6,2] = _b[timedum`i']
			matrix coef[`i'+6,3] = _b[timedum`i'] - _se[timedum`i']*invttail(e(df_r), 0.025)
			matrix coef[`i'+6,4] = _b[timedum`i'] + _se[timedum`i']*invttail(e(df_r), 0.025)

			quietly sum urban if eventime==`i'
			matrix coef[`i'+6,5] = 100*r(mean)
			matrix coef[`i'+6,6] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef[`i'+6,7] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)

			quietly sum stillurban if eventime==`i'
			matrix coef[`i'+6,8] = 100*r(mean)
			matrix coef[`i'+6,9] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef[`i'+6,10] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
		}
	}

  xtreg lninc_h timedum_jakarta_5-timedum_jakarta_2 /// pre in Jakarta
	              timedum_jakarta0-timedum_jakarta5 /// post in Jakarta
								timedum_notjakarta_5-timedum_notjakarta_2 /// pre in not Jakarta
								timedum_notjakarta0-timedum_notjakarta5 /// post in not Jakarta
								endpoint_jakarta_pre endpoint_jakarta_post /// pooled endpoints in Jakarta
								endpoint_notjakarta_pre endpoint_notjakarta_post /// pooled endpoints in Jakarta
								age_sq i.year , ///
	  fe i(pidlink) cluster(pidlink) robust // controls

	matrix coef_jakarta = J(11,10,.)
	matrix coef_notjakarta = J(11,10,.)

	matrix colnames coef_jakarta = eventime /// 1
	  coef_jakarta coef_jakarta_lci coef_jakarta_uci /// 4
	  frac_urban_jakarta frac_urban_jakarta_lci frac_urban_jakarta_uci /// 7
		hazard_cont_jakarta hazard_cont_jakarta_lci hazard_cont_jakarta_uci

	matrix colnames coef_notjakarta = eventime /// 1
	  coef_notjakarta coef_notjakarta_lci coef_notjakarta_uci /// 4
	  frac_urban_notjakarta frac_urban_notjakarta_lci frac_urban_notjakarta_uci /// 7
		hazard_cont_notjakarta hazard_cont_notjakarta_lci hazard_cont_notjakarta_uci

	forvalues i=-5/5 {
		matrix coef_jakarta[`i'+6,1] = `i'
		matrix coef_notjakarta[`i'+6,1] = `i'

		if `i' <= -2 {

		  matrix coef_jakarta[`i'+6,2] = _b[timedum_jakarta_`=-`i'']
			matrix coef_jakarta[`i'+6,3] = _b[timedum_jakarta_`=-`i''] - _se[timedum_jakarta_`=-`i'']*invttail(e(df_r), 0.025)
			matrix coef_jakarta[`i'+6,4] = _b[timedum_jakarta_`=-`i''] + _se[timedum_jakarta_`=-`i'']*invttail(e(df_r), 0.025)

			matrix coef_jakarta[`i'+6,5] = 0
			matrix coef_jakarta[`i'+6,8] = 0
		}
		if `i' == -1 {

		  matrix coef_jakarta[`i'+6,2] = 0

			matrix coef_jakarta[`i'+6,5] = 0
			matrix coef_jakarta[`i'+6,8] = 0
		}
		if `i' >= 0 {

			matrix coef_jakarta[`i'+6,2] = _b[timedum_jakarta`i']
			matrix coef_jakarta[`i'+6,3] = _b[timedum_jakarta`i'] - _se[timedum_jakarta`i']*invttail(e(df_r), 0.025)
			matrix coef_jakarta[`i'+6,4] = _b[timedum_jakarta`i'] + _se[timedum_jakarta`i']*invttail(e(df_r), 0.025)

			quietly sum urban if eventime==`i' & firsturban_jakarta==1
			matrix coef_jakarta[`i'+6,5] = 100*r(mean)
			matrix coef_jakarta[`i'+6,6] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_jakarta[`i'+6,7] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)

			quietly sum stillurban_jakarta if eventime==`i'
			matrix coef_jakarta[`i'+6,8] = 100*r(mean)
			matrix coef_jakarta[`i'+6,9] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_jakarta[`i'+6,10] =100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
		}


		if `i' <= -2 {

		  matrix coef_notjakarta[`i'+6,2] = _b[timedum_notjakarta_`=-`i'']
			matrix coef_notjakarta[`i'+6,3] = _b[timedum_notjakarta_`=-`i''] - _se[timedum_notjakarta_`=-`i'']*invttail(e(df_r), 0.025)
			matrix coef_notjakarta[`i'+6,4] = _b[timedum_notjakarta_`=-`i''] + _se[timedum_notjakarta_`=-`i'']*invttail(e(df_r), 0.025)


			matrix coef_notjakarta[`i'+6,5] = 0
			matrix coef_notjakarta[`i'+6,8] = 0
		}
		if `i' == -1 {

		  matrix coef_notjakarta[`i'+6,2] = 0

			matrix coef_notjakarta[`i'+6,5] = 0
			matrix coef_notjakarta[`i'+6,8] = 0
		}
		if `i' >= 0 {

			matrix coef_notjakarta[`i'+6,2] = _b[timedum_notjakarta`i']
			matrix coef_notjakarta[`i'+6,3] = _b[timedum_notjakarta`i'] - _se[timedum_notjakarta`i']*invttail(e(df_r), 0.025)
			matrix coef_notjakarta[`i'+6,4] = _b[timedum_notjakarta`i'] + _se[timedum_notjakarta`i']*invttail(e(df_r), 0.025)

			quietly sum urban if eventime==`i' & firsturban_jakarta==0
			matrix coef_notjakarta[`i'+6,5] = 100*r(mean)
			matrix coef_notjakarta[`i'+6,6] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_notjakarta[`i'+6,7] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)

			quietly sum stillurban_notjakarta if eventime==`i'
			matrix coef_notjakarta[`i'+6,8] = 100*r(mean)
			matrix coef_notjakarta[`i'+6,9] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_notjakarta[`i'+6,10] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
		}
	}


  xtreg lninc_h timedum_5-timedum_2 /// pre
							  timedum0 /// first period
								timedum_survivor1-timedum_survivor5 /// post for survivors
								timedum_notsurvivor1-timedum_notsurvivor5 /// post for not survivors
								endpoint_pre /// pre end points
								endpoint_survivor_post endpoint_notsurvivor_post /// pooled endpoints for survivors
								age_sq i.year , ///
	  fe i(pidlink) cluster(pidlink) robust // controls


	matrix coef_survivor = J(11,10,.)
	matrix coef_notsurvivor = J(11,10,.)

	matrix colnames coef_survivor = eventime /// 1
	  coef_survivor coef_survivor_lci coef_survivor_uci /// 4
	  frac_urban_survivor frac_urban_survivor_lci frac_urban_survivor_uci /// 7
		hazard_cont_survivor hazard_cont_survivor_lci hazard_cont_survivor_uci

	matrix colnames coef_notsurvivor = eventime /// 1
	  coef_notsurvivor coef_notsurvivor_lci coef_notsurvivor_uci /// 4
	  frac_urban_notsurvivor frac_urban_notsurvivor_lci frac_urban_notsurvivor_uci /// 7
		hazard_cont_notsurvivor hazard_cont_notsurvivor_lci hazard_cont_notsurvivor_uci


	forvalues i=-5/5 {
		matrix coef_survivor[`i'+6,1] = `i'
		matrix coef_notsurvivor[`i'+6,1] = `i'

		if `i' <= -2 {

		  matrix coef_survivor[`i'+6,2] = _b[timedum_`=-`i'']
			matrix coef_survivor[`i'+6,3] = _b[timedum_`=-`i''] - _se[timedum_`=-`i'']*invttail(e(df_r), 0.025)
			matrix coef_survivor[`i'+6,4] = _b[timedum_`=-`i''] + _se[timedum_`=-`i'']*invttail(e(df_r), 0.025)

			matrix coef_survivor[`i'+6,5] = 0
			matrix coef_survivor[`i'+6,8] = 0
		}
		if `i' == -1 {

		  matrix coef_survivor[`i'+6,2] = 0

			matrix coef_survivor[`i'+6,5] = 0
			matrix coef_survivor[`i'+6,8] = 0
		}
		if `i' == 0 {

		  matrix coef_survivor[`i'+6,2] = _b[timedum0]
			matrix coef_survivor[`i'+6,3] = _b[timedum0] - _se[timedum0]*invttail(e(df_r), 0.025)
			matrix coef_survivor[`i'+6,4] = _b[timedum0] + _se[timedum0]*invttail(e(df_r), 0.025)

			quietly sum urban if eventime==`i'
			matrix coef_survivor[`i'+6,5] = 100*r(mean)
			matrix coef_survivor[`i'+6,6] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_survivor[`i'+6,7] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)

			quietly sum stillurban if eventime==`i'
			matrix coef_survivor[`i'+6,8] = 100*r(mean)
			matrix coef_survivor[`i'+6,9] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_survivor[`i'+6,10] =100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
		}
		if `i' > 0 {

			matrix coef_survivor[`i'+6,2] = _b[timedum_survivor`i']
			matrix coef_survivor[`i'+6,3] = _b[timedum_survivor`i'] - _se[timedum_survivor`i']*invttail(e(df_r), 0.025)
			matrix coef_survivor[`i'+6,4] = _b[timedum_survivor`i'] + _se[timedum_survivor`i']*invttail(e(df_r), 0.025)

			quietly sum urban if eventime==`i'
			matrix coef_survivor[`i'+6,5] = 100*r(mean)
			matrix coef_survivor[`i'+6,6] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_survivor[`i'+6,7] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)

			quietly sum stillurban if eventime==`i'
			matrix coef_survivor[`i'+6,8] = 100*r(mean)
			matrix coef_survivor[`i'+6,9] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_survivor[`i'+6,10] =100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
		}

		if `i' == 0 {
		  matrix coef_notsurvivor[`i'+6,2] = _b[timedum0]
			matrix coef_notsurvivor[`i'+6,3] = _b[timedum0] - _se[timedum0]*invttail(e(df_r), 0.025)
			matrix coef_notsurvivor[`i'+6,4] = _b[timedum0] + _se[timedum0]*invttail(e(df_r), 0.025)

		}
		if `i' > 0 {

			matrix coef_notsurvivor[`i'+6,2] = _b[timedum_notsurvivor`i']
			matrix coef_notsurvivor[`i'+6,3] = coef_notsurvivor[`i'+6,2] - _se[timedum_notsurvivor`i']*invttail(e(df_r), 0.025)
			matrix coef_notsurvivor[`i'+6,4] = coef_notsurvivor[`i'+6,2] + _se[timedum_notsurvivor`i']*invttail(e(df_r), 0.025)

		}
	}

	clear

	svmat coef , names(col)

	label var eventime "Years relative to first urban move"
*** Graph event study
	graph twoway ///
		(line coef_lci eventime, lwidth(thin) lcolor(black) lpattern(dash) cmissing(n)) ///
		(line coef_uci eventime, lwidth(thin) lcolor(black) lpattern(dash) cmissing(n)) ///
	  (line coef eventime, lcolor(black) lwidth(medthick)) ///
		(pcarrowi 0.8 -1.2 0.8 -1.7, lcolor(black) mcolor(black) mlabcolor(black)) ///
		(pcarrowi 0.8 -0.8 0.8 -0.3, lcolor(black) mcolor(black) mlabcolor(black)), /// arrows
		text(0.88 -1.54 "Rural") ///
		text(0.88 -0.4 "Urban") ///
		xline(-1, lcolor(black) lpattern(dot)) ///
		yline(0, lcolor(black) lwidth(thin)) ///
		xlabel(-5(1)5, grid nolabels) ///
		ylabel(-1(0.5)1, nogrid) ///
		graphregion(color(white) lcolor(white)) ///
		legend(off) ///
		xtitle("") ///
		ytitle("Urban Wage Gap (log points)") ///
		name(eventstudy_ifls, replace) ///
		nodraw
*** Graph Survival Rate in Urban areas
	graph twoway ///
		(line hazard_cont_lci eventime if eventime >= 0 , lwidth(thin) lcolor(black) lpattern(dash) cmissing(n)) ///
		(line hazard_cont_uci eventime if eventime >= 0 , lwidth(thin) lcolor(black) lpattern(dash) cmissing(n)) ///
	  (line hazard_cont eventime if eventime >= 0 , lcolor(black) lwidth(medthick)) , ///
		xline(-1, lcolor(black) lpattern(dot)) ///
		xlabel(-5(1)5, grid) ///
		ylabel(0(20)100, nogrid) ///
		graphregion(color(white) lcolor(white)) ///
		legend(off) ///
		ytitle("Urban Survival Rate (percent)") ///
		name(survival_ifls, replace) ///
		nodraw

  graph set eps fontface Garamond
	graph set window fontface Garamond

  graph combine eventstudy_ifls survival_ifls , ///
	  rows(2) ///
		imargin(0 0 0 0) ///
		graphregion(color(white) lcolor(white)) ///
		ysize(6)

	graph export "Figure3_IFLS.eps", replace

	graph set window fontface default
	graph set eps fontface default

	restore
