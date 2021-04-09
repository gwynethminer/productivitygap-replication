preserve
*** Education dummies
  gen educpri = educlv >= 1 if !missing(educlv)
  gen educsec = educlv >= 3 if !missing(educlv)
  gen educcol = educlv == 4 if !missing(educlv)
*** Different categories
  *** Always rural
  egen always_rural = max(max(urban,urban_birth)), by(pidlink)
  replace always_rural = 1 - always_rural
  *** Always urban
  egen always_urban = min(min(urban,urban_birth)), by(pidlink)
  *** Rural non-migrants
	  gen rural_nonmigrants = migr_ever == 0 & always_rural == 1
  *** Rural-to-Rural migrants
	  gen migrRR_ever = always_rural==1 & migr_ever==1
  *** Rural-to-Urban migrants
  gen migrRU_ever = always_rural==0 & urban_birth==0
  gen migrUR_ever = always_urban==0 & urban_birth==1

  egen nonag_ever = max(nonag) , by(pidlink)
*** Collapse the data to the individual level
  collapse (first) ravens_norm educpri educsec educcol female ///
    always_rural migrRU_ever migrUR_ever always_urban ///
	  nonag_ever urban_birth, by(pidlink)

  label var ravens_norm "Raven's Z-score"
  label var educpri "Primary Ed."
  label var educsec "Secondary Ed."
  label var educcol "College"
  label var female "Female"
  label var always_rural "Always Rural"
  label var migrRU_ever "Rural-to-Urban Migrants"
  label var migrUR_ever "Urban-to-Rural Migrants"
  label var always_urban "Always Urban"
  label var nonag_ever "Ever Non-ag"

  eststo all: estpost sum educpri educsec educcol female ravens_norm
  eststo always_rural: estpost sum educpri educsec educcol female ravens_norm if always_rural==1
  eststo migrRU_ever: estpost sum educpri educsec educcol female ravens_norm if migrRU_ever==1
  eststo migrUR_ever: estpost sum educpri educsec educcol female ravens_norm if migrUR_ever==1
  eststo always_urban: estpost sum educpri educsec educcol female ravens_norm if always_urban==1

  eststo counts: estpost sum educpri educsec educcol female ravens_norm

  quietly count
  local num_all = r(N)
  quietly count if always_rural==1
  local num_always_rural = r(N)
  quietly count if migrRU_ever==1
  local num_migrRU_ever = r(N)
  quietly count if migrUR_ever==1
  local num_migrUR_ever = r(N)
  quietly count if always_urban==1
  local num_always_urban = r(N)

  esttab all always_rural migrRU_ever always_urban migrUR_ever counts ///
    using "results/tables/Table2_IFLS.tex", replace type fragment ///
	cells(mean(pattern(1 1 1 1 1 0) fmt(%9.3f))& ///
          count(pattern(0 0 0 0 0 1) fmt(%9.0f)) ///
	      sd(par([ ]) pattern(1 1 1 1 1 0) fmt(%9.3f))) ///
    noobs ///
	title("Indonesia") ///
	mlabels("N=`num_all'" ///
			"N=`num_always_rural'" ///
	        "N=`num_migrRU_ever'" ///
			"N=`num_always_urban'" ///
			"N=`num_migrUR_ever'" ///
			"") ///
	nonumbers ///
  mgroups("All" ///
	        "Always Rural" ///
			    "\shortstack{Rural-to-Urban\\Migrants}" ///
		      "Always Urban" ///
			    "\shortstack{Urban-to-Rural\\Migrants}" ///
			    "Obs" , ///
	  pattern(1 1 1 1 1 1) ///
		span ///
		prefix(\multicolumn{@span}{c}{) ///
		suffix(})) ///
	collabels(none) ///
	label ///
	legend ///
	booktabs
