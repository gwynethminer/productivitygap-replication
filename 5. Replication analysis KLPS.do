/*******************************************************************************

Do-file "5. Replication analysis KLPS.do"

Replication code for
Joan Hamory, Marieke Kleemans, Nicholas Y. Li, and Edward Miguel
Reevaluating Agricultural Productivity Gaps with Longitudinal Microdata
Created Aug 2020 using Stata version 14.2

This do-file uses the 4 datasets created by do-file "4. Creation of KLPS
replication data.do" to replicate all figures and tables that use KLPS data

Please contact Marieke Kleemans at kleemans@illinois.edu for questions

*******************************************************************************/

********************************************************************************
*** Figure 1: Productivity Gap in Total Earnings
********************************************************************************
do "do/analysis/main/figures/figure1_KLPS.do"

*** Combine figures ****

** End Figure Code Nonag Indonesia
	graph set eps fontface Garamond

* graph combine "$folderIFLS\ind_nonag.gph" "$folderIFLS\ind_urban.gph" "ken_nonag.gph" "ken_urban.gph" , ///

graph combine Figure1_IFLS_agnonag.gph Figure1_IFLS_ruralurban.gph Figure1_KLPS_agnonag.gph Figure1_KLPS_ruralurban.gph , ///
		xsize(12) ///
		ysize(8) ///
		imargin(0 0 0 0) ///
		graphregion(color(white) lcolor(white))

graph save "Figure1.gph" , replace

		graph export "Figure1.eps" , replace

	graph set eps fontface default

********************************************************************************
*** Figure 3: Event Study of Urban Migration
********************************************************************************
do "do/analysis/main/figures/figure3_KLPS.do"

********************************************************************************
*** Table 1: Non-Agriculture/Agriculture and Urban/Rural
********************************************************************************
do "do/analysis/main/tables/table1_KLPS.do"

********************************************************************************
*** Table 2_KLPS: Summary Statistics
********************************************************************************
do "do/analysis/main/tables/table2_KLPS.do"

********************************************************************************
*** Table 3: Correlates of Employment in Non-Agriculture and Urban Migration
********************************************************************************
do "do/analysis/main/tables/table3_KLPS.do"

********************************************************************************
*** Table 4: Non-Agricultural/Agricultural Gap in Earnings
********************************************************************************
do "do/analysis/main/tables/table4_KLPS.do"

********************************************************************************
*** Table 5: Urban/Rural Gap in Earnings
********************************************************************************
do "do/analysis/main/tables/table5_KLPS.do"


********************************************************************************
*** Appendix Figures and Tables
********************************************************************************
do "do/analysis/appendix/appendix_tables_figures_KLPS.do"
