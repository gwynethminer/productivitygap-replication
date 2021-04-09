/*******************************************************************************

Do-file "3. Replication analysis IFLS.do"

Replication code for
Joan Hamory, Marieke Kleemans, Nicholas Y. Li, and Edward Miguel
Reevaluating Agricultural Productivity Gaps with Longitudinal Microdata
Created Apr 2021 using Stata version 15

This do-file uses the 3 datasets created by do-file "2. Creation of IFLS
replication data.do" to replicate all figures and tables that use IFLS data

Please contact Marieke Kleemans at kleemans@illinois.edu for questions

*******************************************************************************/



********************************************************************************
*** Figure 1: Productivity Gap in Total Earnings
********************************************************************************
do "$do/analysis/main/figures/figure1_IFLS.do"

********************************************************************************
*** Figure 3: Event Study of Urban Migration
********************************************************************************
do "$do/analysis/main/figures/figure3_IFLS.do"

********************************************************************************
*** Table 1: Non-Agriculture/Agriculture and Urban/Rural
********************************************************************************
do "$do/analysis/main/tables/table1_IFLS.do"

********************************************************************************
*** Table 2_IFLS: Summary Statistics
********************************************************************************
do "$do/analysis/main/tables/table2_IFLS.do"

********************************************************************************
*** Table 3: Correlates of Employment in Non-Agriculture and Urban Migration
********************************************************************************
**creates data to be used later when running the KLPS analysis
keep pidlink migrRU_ever nonag_ever educpri educsec educcol female ravens_norm always_rural
save "$da/Table3_IFLS.dta", replace
restore


********************************************************************************
*** Table 4: Non-Agricultural/Agricultural Gap in Earnings
********************************************************************************
do "$do/analysis/main/tables/table4_IFLS.do"

********************************************************************************
*** Table 5: Urban/Rural Gap in Earnings
********************************************************************************
do "$do/analysis/main/tables/table5_IFLS.do"

********************************************************************************
*** Table 6: Gap in Earnings, Indonesia. For Individuals Born Rural and Urban
********************************************************************************
do "$do/analysis/main/tables/table6_IFLS.do"

********************************************************************************
*** Appendix Figures and Tables
********************************************************************************
do "$do/analysis/appendix/appendix_tables_figures_IFLS.do"

exit
