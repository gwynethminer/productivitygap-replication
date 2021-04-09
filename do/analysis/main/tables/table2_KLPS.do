********************************************************************************
*** Table 2_KLPS: Summary Statistics
********************************************************************************
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


  local covs educpri educsec educcol female ravens

  eststo all: estpost sum `covs'
  eststo neverU: estpost sum `covs' if migrU_ever==0
  eststo migrU: estpost sum `covs' if migrU_ever==1

  eststo counts: estpost sum `covs' if !missing(migrU_ever)

  * Columnwise counts
  quietly count
  local num_total = r(N)
  quietly count if migrU_ever==0
  local num_neverU = r(N)
  quietly count if migrU_ever==1
  local num_migrU = r(N)


  esttab all neverU migrU migrU migrU counts ///
    using "results/tables/Table2_KLPS.tex", replace type fragment ///
	  cells(mean(pattern(1 1 1 0 0 0) fmt(%9.3f))& ///
          count(pattern(0 0 0 0 0 1) fmt(%9.0f)) ///
	        sd(par([ ]) pattern(1 1 1 0 0 0) fmt(%9.3f))) ///
	  noobs ///
		title("Kenya") ///
		mlabels("N=`num_total'" ///
						"N=`num_neverU'" ///
				    "N=`num_migrU'" ///
						"" ///
						"" ///
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
