********************************************************************************
*** Figure 3: Event Study of Urban Migration
********************************************************************************
preserve
xtset pupid yrmth

*** Define move rural to urban
gen moveR2U = 1 if urban == 1 & l.urban == 0
gen temp = yrmth if moveR2U == 1
egen firstmoveR2U = min(temp) , by(pupid)
drop temp

* eventime
gen eventime = yrmth - firstmoveR2U

* generate event time dummies
	forvalues i = -60/60 {
		if `i' < 0 {
			gen timedum_`=-`i'' = eventime==`i'
		}
		else {
			gen timedum`i' = eventime==`i'
		}
	}

	* Event time -6 and 6 are endpoints
	gen endpoint_pre = eventime < -60
	gen endpoint_post = eventime > 60
	replace endpoint_post = 0 if missing(eventime)

	* Create continous stay dummy
	* Find first rural observation in post-eventime=0
	* stillurban is on before that and off after
	gen temp = eventime if urban==0 & eventime > 0
	egen first_postrural = min(temp) , by(pupid)
	drop temp

	gen stillurban = eventime < first_postrural & eventime >= 0


	* Partition event time dummies into Jakarta and not Jakarta
	gen temp = nairobi if eventime==0
	egen firsturban_nairobi = mean(temp), by(pupid)
	drop temp


	forvalues i=-60/60 {
		if `i' < 0 {
			gen timedum_nairobi_`=-`i'' = eventime==`i'&(firsturban_nairobi==1)
			gen timedum_notnairobi_`=-`i'' = eventime==`i'&(firsturban_nairobi==0)
		}
		else {
			gen timedum_nairobi`i' = eventime==`i'&(firsturban_nairobi==1)
			gen timedum_notnairobi`i' = eventime==`i'&(firsturban_nairobi==0)
		}
	}

	gen endpoint_nairobi_pre = endpoint_pre&(firsturban_nairobi==1)
	gen endpoint_notnairobi_pre = endpoint_pre&(firsturban_nairobi==0)

	gen endpoint_nairobi_post = endpoint_post&(firsturban_nairobi==1)
	gen endpoint_notnairobi_post = endpoint_post&(firsturban_nairobi==0)

	* Create continous stay dummy for nairobi
	gen stillurban_nairobi = stillurban if firsturban_nairobi==1
	gen stillurban_notnairobi = stillurban if firsturban_nairobi==0

	* Create interactions with urban for periods greater than 1
	* for survivor inforinc_h
	forvalues i=1/60 {
			gen timedum_survivor`i' = eventime==`i' & (stillurban==1)
			gen timedum_notsurvivor`i' = eventime==`i' & (stillurban==0)
  }

	gen endpoint_survivor_post = endpoint_post & (stillurban==1)
	gen endpoint_notsurvivor_post = endpoint_post & (stillurban==0)

	order timedum_notnairobi* , after(endpoint_nairobi_post)
	order endpoint_notnairobi* , after(timedum_notnairobi60)
	order stillurban_*nairobi , after(endpoint_notnairobi_post)
	order timedum_survivor*, after(stillurban_notnairobi)
	order timedum_notsurvivor*, after(timedum_survivor60)
	order endpoint_survivor*, after(timedum_notsurvivor60)
*
  xtreg lninc_h timedum_60-timedum_2 /// pre
	              timedum0-timedum60 /// post
								endpoint_pre endpoint_post /// pooled endpoints
								age_sq i.yrmth , ///
	  fe i(pupid) cluster(pupid) robust // controls

	** Count number of individuals
	* At time period of move
	count if timedum0 == 1
	* After 5 years
	count if timedum60 == 1

	* Create matrix of coefficients and upper and lower CI
	matrix coef = J(121,10,.)
	matrix colnames coef = eventime /// 1
	  coef coef_lci coef_uci /// 4
	  frac_urban frac_urban_lci frac_urban_uci /// 7
		hazard_cont hazard_cont_lci hazard_cont_uci
	forvalues i=-60/60 {
		matrix coef[`i'+61,1] = `i'

		if `i' <= -2 {
		  *eventime coefficients
		  matrix coef[`i'+61,2] = _b[timedum_`=-`i'']
			matrix coef[`i'+61,3] = _b[timedum_`=-`i''] - _se[timedum_`=-`i'']*invttail(e(df_r), 0.025)
			matrix coef[`i'+61,4] = _b[timedum_`=-`i''] + _se[timedum_`=-`i'']*invttail(e(df_r), 0.025)

			*hazards and fractions
			matrix coef[`i'+61,5] = 0
			matrix coef[`i'+61,8] = 0
		}
		if `i' == -1 {
		  *eventime coefficients
		  matrix coef[`i'+61,2] = 0
			*hazards and fractions
			matrix coef[`i'+61,5] = 0
			matrix coef[`i'+61,8] = 0
		}
		if `i' >= 0 {
		  *eventime coefficients
			matrix coef[`i'+61,2] = _b[timedum`i']
			matrix coef[`i'+61,3] = _b[timedum`i'] - _se[timedum`i']*invttail(e(df_r), 0.025)
			matrix coef[`i'+61,4] = _b[timedum`i'] + _se[timedum`i']*invttail(e(df_r), 0.025)
			*fraction urban
			* include max/min so lower ci doesn't go below zero or above 1
			quietly sum urban if eventime==`i'
			matrix coef[`i'+61,5] = 100*r(mean)
			matrix coef[`i'+61,6] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef[`i'+61,7] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			*urban survival
			* include max so lower ci doesn't go below zero or above 1
			quietly sum stillurban if eventime==`i'
			matrix coef[`i'+61,8] = 100*r(mean)
			matrix coef[`i'+61,9] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef[`i'+61,10] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
		}
	}


  xtreg lninc_h timedum_nairobi_60-timedum_nairobi_2 /// pre in Jakarta
	              timedum_nairobi0-timedum_nairobi60 /// post in Jakarta
								timedum_notnairobi_60-timedum_notnairobi_2 /// pre in not Jakarta
								timedum_notnairobi0-timedum_notnairobi60 /// post in not Jakarta
								endpoint_nairobi_pre endpoint_nairobi_post /// pooled endpoints in Jakarta
								endpoint_notnairobi_pre endpoint_notnairobi_post /// pooled endpoints in Jakarta
								age_sq i.yrmth , ///
	  fe i(pupid) cluster(pupid) robust // controls

	* Create matrix of coefficients and upper and lower CI for Jakarta and Not Jakarta
	matrix coef_nairobi = J(121,10,.)
	matrix coef_notnairobi = J(121,10,.)

	matrix colnames coef_nairobi = eventime /// 1
	  coef_nairobi coef_nairobi_lci coef_nairobi_uci /// 4
	  frac_urban_nairobi frac_urban_nairobi_lci frac_urban_nairobi_uci /// 7
		hazard_cont_nairobi hazard_cont_nairobi_lci hazard_cont_nairobi_uci

	matrix colnames coef_notnairobi = eventime /// 1
	  coef_notnairobi coef_notnairobi_lci coef_notnairobi_uci /// 4
	  frac_urban_notnairobi frac_urban_notnairobi_lci frac_urban_notnairobi_uci /// 7
		hazard_cont_notnairobi hazard_cont_notnairobi_lci hazard_cont_notnairobi_uci


	forvalues i=-60/60 {
		matrix coef_nairobi[`i'+61,1] = `i'
		matrix coef_notnairobi[`i'+61,1] = `i'


		** Nairobi
		if `i' <= -2 {
		  *eventime coefficients
		  matrix coef_nairobi[`i'+61,2] = _b[timedum_nairobi_`=-`i'']
			matrix coef_nairobi[`i'+61,3] = _b[timedum_nairobi_`=-`i''] - _se[timedum_nairobi_`=-`i'']*invttail(e(df_r), 0.025)
			matrix coef_nairobi[`i'+61,4] = _b[timedum_nairobi_`=-`i''] + _se[timedum_nairobi_`=-`i'']*invttail(e(df_r), 0.025)


			*hazards and fractions
			matrix coef_nairobi[`i'+61,5] = 0
			matrix coef_nairobi[`i'+61,8] = 0
		}
		if `i' == -1 {
		  *eventime coefficients
		  matrix coef_nairobi[`i'+61,2] = 0
			*hazards and fractions
			matrix coef_nairobi[`i'+61,5] = 0
			matrix coef_nairobi[`i'+61,8] = 0
		}
		if `i' >= 0 {
		  *eventime coefficients
			matrix coef_nairobi[`i'+61,2] = _b[timedum_nairobi`i']
			matrix coef_nairobi[`i'+61,3] = _b[timedum_nairobi`i'] - _se[timedum_nairobi`i']*invttail(e(df_r), 0.025)
			matrix coef_nairobi[`i'+61,4] = _b[timedum_nairobi`i'] + _se[timedum_nairobi`i']*invttail(e(df_r), 0.025)
			*fraction urban
			* include max/min so lower ci doesn't go below zero or above 1
			quietly sum urban if eventime==`i' & firsturban_nairobi==1
			matrix coef_nairobi[`i'+61,5] = 100*r(mean)
			matrix coef_nairobi[`i'+61,6] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_nairobi[`i'+61,7] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			*urban survival
			* include max so lower ci doesn't go below zero or above 1
			quietly sum stillurban_nairobi if eventime==`i'
			matrix coef_nairobi[`i'+61,8] = 100*r(mean)
			matrix coef_nairobi[`i'+61,9] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_nairobi[`i'+61,10] =100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
		}


		** Not Nairobi
		if `i' <= -2 {
		  *eventime coefficients
		  matrix coef_notnairobi[`i'+61,2] = _b[timedum_notnairobi_`=-`i'']
			matrix coef_notnairobi[`i'+61,3] = _b[timedum_notnairobi_`=-`i''] - _se[timedum_notnairobi_`=-`i'']*invttail(e(df_r), 0.025)
			matrix coef_notnairobi[`i'+61,4] = _b[timedum_notnairobi_`=-`i''] + _se[timedum_notnairobi_`=-`i'']*invttail(e(df_r), 0.025)


			*hazards and fractions
			matrix coef_notnairobi[`i'+61,5] = 0
			matrix coef_notnairobi[`i'+61,8] = 0
		}
		if `i' == -1 {
		  *eventime coefficients
		  matrix coef_notnairobi[`i'+61,2] = 0
			*hazards and fractions
			matrix coef_notnairobi[`i'+61,5] = 0
			matrix coef_notnairobi[`i'+61,8] = 0
		}
		if `i' >= 0 {
		  *eventime coefficients
			matrix coef_notnairobi[`i'+61,2] = _b[timedum_notnairobi`i']
			matrix coef_notnairobi[`i'+61,3] = _b[timedum_notnairobi`i'] - _se[timedum_notnairobi`i']*invttail(e(df_r), 0.025)
			matrix coef_notnairobi[`i'+61,4] = _b[timedum_notnairobi`i'] + _se[timedum_notnairobi`i']*invttail(e(df_r), 0.025)
			*fraction urban
			* include max/min so lower ci doesn't go below zero or above 1
			quietly sum urban if eventime==`i' & firsturban_nairobi==0
			matrix coef_notnairobi[`i'+61,5] = 100*r(mean)
			matrix coef_notnairobi[`i'+61,6] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_notnairobi[`i'+61,7] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			*urban survival
			* include max so lower ci doesn't go below zero or above 1
			quietly sum stillurban_notnairobi if eventime==`i'
			matrix coef_notnairobi[`i'+61,8] = 100*r(mean)
			matrix coef_notnairobi[`i'+61,9] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_notnairobi[`i'+61,10] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
		}
	}


  xtreg lninc_h timedum_60-timedum_2 /// pre
							  timedum0 /// first period
								timedum_survivor1-timedum_survivor60 /// post for survivors
								timedum_notsurvivor1-timedum_notsurvivor60 /// post for not survivors
								endpoint_pre /// pre end points
								endpoint_survivor_post endpoint_notsurvivor_post /// pooled endpoints for survivors
								age_sq i.yrmth , ///
	  fe i(pupid) cluster(pupid) robust // controls


	matrix coef_survivor = J(121,10,.)
	matrix coef_notsurvivor = J(121,10,.)

	matrix colnames coef_survivor = eventime /// 1
	  coef_survivor coef_survivor_lci coef_survivor_uci /// 4
	  frac_urban_survivor frac_urban_survivor_lci frac_urban_survivor_uci /// 7
		hazard_cont_survivor hazard_cont_survivor_lci hazard_cont_survivor_uci

	matrix colnames coef_notsurvivor = eventime /// 1
	  coef_notsurvivor coef_notsurvivor_lci coef_notsurvivor_uci /// 4
	  frac_urban_notsurvivor frac_urban_notsurvivor_lci frac_urban_notsurvivor_uci /// 7
		hazard_cont_notsurvivor hazard_cont_notsurvivor_lci hazard_cont_notsurvivor_uci


	forvalues i=-60/60 {
		matrix coef_survivor[`i'+61,1] = `i'
		matrix coef_notsurvivor[`i'+61,1] = `i'


		** Survivor
		if `i' <= -2 {
		  *eventime coefficients
		  matrix coef_survivor[`i'+61,2] = _b[timedum_`=-`i'']
			matrix coef_survivor[`i'+61,3] = _b[timedum_`=-`i''] - _se[timedum_`=-`i'']*invttail(e(df_r), 0.025)
			matrix coef_survivor[`i'+61,4] = _b[timedum_`=-`i''] + _se[timedum_`=-`i'']*invttail(e(df_r), 0.025)


			*hazards and fractions
			matrix coef_survivor[`i'+61,5] = 0
			matrix coef_survivor[`i'+61,8] = 0
		}
		if `i' == -1 {
		  *eventime coefficients
		  matrix coef_survivor[`i'+61,2] = 0
			*hazards and fractions
			matrix coef_survivor[`i'+61,5] = 0
			matrix coef_survivor[`i'+61,8] = 0
		}
		if `i' == 0 {
			*eventime coefficients
		  matrix coef_survivor[`i'+61,2] = _b[timedum0]
			matrix coef_survivor[`i'+61,3] = _b[timedum0] - _se[timedum0]*invttail(e(df_r), 0.025)
			matrix coef_survivor[`i'+61,4] = _b[timedum0] + _se[timedum0]*invttail(e(df_r), 0.025)

			* Hazards and fractions
			* include max/min so lower ci doesn't go below zero or above 1
			quietly sum urban if eventime==`i'
			matrix coef_survivor[`i'+61,5] = 100*r(mean)
			matrix coef_survivor[`i'+61,6] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_survivor[`i'+61,7] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			*urban survival
			* include max so lower ci doesn't go below zero or above 1
			quietly sum stillurban if eventime==`i'
			matrix coef_survivor[`i'+61,8] = 100*r(mean)
			matrix coef_survivor[`i'+61,9] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_survivor[`i'+61,10] =100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
		}
		if `i' > 0 {
		  *eventime coefficients
			matrix coef_survivor[`i'+61,2] = _b[timedum_survivor`i']
			matrix coef_survivor[`i'+61,3] = _b[timedum_survivor`i'] - _se[timedum_survivor`i']*invttail(e(df_r), 0.025)
			matrix coef_survivor[`i'+61,4] = _b[timedum_survivor`i'] + _se[timedum_survivor`i']*invttail(e(df_r), 0.025)
			*fraction urban
			* include max/min so lower ci doesn't go below zero or above 1
			quietly sum urban if eventime==`i'
			matrix coef_survivor[`i'+61,5] = 100*r(mean)
			matrix coef_survivor[`i'+61,6] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_survivor[`i'+61,7] = 100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			*urban survival
			* include max so lower ci doesn't go below zero or above 1
			quietly sum stillurban if eventime==`i'
			matrix coef_survivor[`i'+61,8] = 100*r(mean)
			matrix coef_survivor[`i'+61,9] = 100*min(max(r(mean) - r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
			matrix coef_survivor[`i'+61,10] =100*min(max(r(mean) + r(sd)/sqrt(r(N))*invttail(r(N)-1, 0.025),0),1)
		}

		** Not Survivor
		if `i' == 0 {
		  matrix coef_notsurvivor[`i'+61,2] = _b[timedum0]
			matrix coef_notsurvivor[`i'+61,3] = _b[timedum0] - _se[timedum0]*invttail(e(df_r), 0.025)
			matrix coef_notsurvivor[`i'+61,4] = _b[timedum0] + _se[timedum0]*invttail(e(df_r), 0.025)

		}
		if `i' > 0 {
		  *eventime coefficients
			matrix coef_notsurvivor[`i'+61,2] = _b[timedum_notsurvivor`i']
			matrix coef_notsurvivor[`i'+61,3] = _b[timedum_notsurvivor`i'] - _se[timedum_notsurvivor`i']*invttail(e(df_r), 0.025)
			matrix coef_notsurvivor[`i'+61,4] = _b[timedum_notsurvivor`i'] + _se[timedum_notsurvivor`i']*invttail(e(df_r), 0.025)

		}
	}

  ****************************
	clear

	svmat coef , names(col)

	label var eventime "Years relative to first urban move"
	replace eventime = eventime/12

	* Graph event study
	graph twoway ///
		(line coef_lci eventime, lwidth(thin) lcolor(black) lpattern(dash) cmissing(n)) ///
		(line coef_uci eventime, lwidth(thin) lcolor(black) lpattern(dash) cmissing(n)) ///
	  (line coef eventime, lcolor(black) lwidth(medthick)) ///
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
		name(eventstudy_klps, replace) ///
		nodraw

	* Graph Survival Rate in Urban areas
	graph twoway ///
		(line hazard_cont_lci eventime if eventime >= 0, lwidth(thin) lcolor(black) lpattern(dash) cmissing(n)) ///
		(line hazard_cont_uci eventime if eventime >= 0, lwidth(thin) lcolor(black) lpattern(dash) cmissing(n)) ///
	  (line hazard_cont eventime if eventime >= 0, lcolor(black) lwidth(medthick)) , ///
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

  graph combine eventstudy_klps survival_klps , ///
	  rows(2) ///
		imargin(0 0 0 0) ///
		graphregion(color(white) lcolor(white)) ///
		ysize(6)

	graph export "Figure3_KLPS.eps", replace

	graph set window fontface default
	graph set eps fontface default

restore
