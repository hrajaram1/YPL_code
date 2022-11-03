/*******************************************************************************
Purpose: 
Summary stats by treatment cohort table for new sample 
a.	One column for each treatment cohort.  One column for our control set.  One column for schools that we exclude from the control set for any reason. 
b.	Use stats from 2014-2015 if available. If not, use the first year available in the data. egen minyr=min(year), by(bcode)

Created on: 12/19/2019
By: Hersheena Rajaram

Last modified on: 4/2/2020
By: Hersheena Rajaram
    
*******************************************************************************/

clear all
set more off
macro drop _all
cap log close
program drop _all
matrix drop _all
set trace on
set tracedepth 1

global date = c(current_date)
global username = c(username)

** File paths
global base "Y:\projects\MCAN"
global epi_base "O:\data_final"
global mcan_data "$base\data_final"
global temp_data "$base\data_temp"
global mcan_raw "$base\data_raw"
global mcan_log "$base\code_logs"
global results "$base\results"


log using "${mcan_log}\summary_stats_${username}.log", replace 

*use "$mcan_data/mcan_school_year_subgroup", clear
use "$mcan_data\pre_analysis_student_level.dta", clear

*keep if subgroup=="all"
global stu_covs "female black hisp asianamer poor sped lep mathg8 readingg8 mathg8_miss readingg8_miss poor_miss sped_miss lep_miss nstu"
global sch_covs "school_nstu school_nstu_sq school_black school_hisp school_poor school_sped school_mathg8" 
global sch_fix "charter magnet city suburb town rural altconfig altsch vocsch"
global covs "female black hisp asianamer poor sped lep mathg8 readingg8 mathg8_miss readingg8_miss poor_miss sped_miss lep_miss school_nstu charter magnet urb_city urb_suburb urb_town urb_rural"
* First create a new tag to idenitfy schools in the subgroup 'all'
sort bcode year
egen tag=tag(bcode)
count if tag==1		//There are 1,582 schools in this data

/*****************************************************************************
					TABLE 1 - Stats by Treat Cohort 
******************************************************************************/
putexcel set "$results\MCAN_summary_and_analysis_11132020.xlsx", sh("Stats by Treat Cohort") modify

local ncol = 2
local col: word `ncol' of `c(ALPHA)'
local row = 3

* Number of schools in control
count if potential_control==1 & tag==1
putexcel `col'`row' = `r(N)'
local ++ncol
local col: word `ncol' of `c(ALPHA)'

* Number of schools in 2016,2017,2018 treatment cohort
foreach fy in 2016 2017 2018 {
	count if tag==1 & firstyear==`fy' & treatment_school==1
	putexcel `col'`row' = `r(N)'
	local ++ncol
	local col: word `ncol' of `c(ALPHA)'
}
* Number of schools in 2019 and 2020 treatment cohort (combined)
count if (firstyear==2019 & tag==1)|(firstyear==2020 & tag==1)
putexcel `col'`row' = `r(N)'
local ++ncol
local col: word `ncol' of `c(ALPHA)'

*Number of schools that received treatment by other programs.
count if tag==1 & program_other==1
putexcel `col'`row' = `r(N)'
local ++ncol
local col: word `ncol' of `c(ALPHA)'

*Number of target schools that never received any advising (meri data)
count if tag==1 & control_2==1
putexcel `col'`row' = `r(N)'

local ncol = 2
local col: word `ncol' of `c(ALPHA)'
local row = 6

*WEIGHTED STUDENT COVARIATES
* For control schools
foreach s in $covs {
	sum `s' if potential_control==1 [aw=nstu],d
	putexcel `col'`row' = `r(mean)'
	local row=`row'+1
}

local ++ncol
local col:word `ncol' of `c(ALPHA)'
local row = 6

foreach fy in 2016 2017 2018 {
	foreach s in $covs {
	sum `s' if firstyear==`fy' & treatment_school==1 [aw=nstu], d
		putexcel `col'`row' = `r(mean)'	
	local row=`row'+1
	}
local ++ncol
local col: word `ncol' of `c(ALPHA)'
local row = 6
}

* For combined 2019,2020 treatment cohort
foreach s in $covs {
	sum `s' if firstyear==2019|firstyear==2020 [aw=nstu],d
	putexcel `col'`row' = `r(mean)'
	local row=`row'+1
}

local ++ncol
local col:word `ncol' of `c(ALPHA)'
local row = 6

*For schools ever treated by other programs
foreach s in $covs {
	sum `s' if program_other==1 [aw=nstu],d
	putexcel `col'`row' = `r(mean)'
	local row=`row'+1
}

local ++ncol
local col:word `ncol' of `c(ALPHA)'
local row = 6

*For target schools that never received advising
foreach s in $covs {
	sum `s' if control_2==1 [aw=nstu],d
	putexcel `col'`row' = `r(mean)'
	local row=`row'+1
}

/*******************************************************************************
						Number of Students per Advisor
*******************************************************************************/
/*Let's look at some summary stats for number of students by advisor. The code below outputs a table showing 
the mean, median, 10th and 90th percentile of number of students per advisor.*/

* Number of students per advisor = number of students (nstu)/number of advisor(fte)
gen nstu_advisor = nstu/timespent

putexcel set "$results\MCAN_summary_and_analysis_11132020.xlsx", sh("Students_Advisor by Cohort") modify

local ncol = 2
local col: word `ncol' of `c(ALPHA)'
local row = 4

* Number of treatment schools by cohort
foreach fy in 2016 2017 2018 2019 2020 {
	count if tag==1 & firstyear==`fy' 
	putexcel `col'`row' = `r(N)'
	local row = `row'+1
}

* Sum stats by treatment cohort (2012-2019)
local ncol = 3
local col: word `ncol' of `c(ALPHA)'
local row = 4

foreach i in 2016 2017 2018 2019 2020 {
	sum nstu_advisor if firstyear == `i',d
	putexcel `col'`row' = `r(mean)'
	local ++ncol
	local col: word `ncol' of `c(ALPHA)'
	putexcel `col'`row' = `r(p50)'
	local ++ncol
	local col: word `ncol' of `c(ALPHA)'
	putexcel `col'`row' = `r(p10)'
	local ++ncol
	local col: word `ncol' of `c(ALPHA)'
	putexcel `col'`row' = `r(p90)'
	local row = `row'+1
	local ncol = 3
	local col: word `ncol' of `c(ALPHA)'
}

log close

/* Unused code

***CREATE SOME ADDITIONAL VARIABLES
* Urbanicity: create dummy variables to indicate city, suburb, town, and rural
gen urb_city= (urbanicity==1) if !missing(urbanicity)
gen urb_suburb= (urbanicity==2) if !missing(urbanicity)
gen urb_town= (urbanicity==3) if !missing(urbanicity)
gen urb_rural= (urbanicity==4) if !missing(urbanicity)
 
/* School type
gen type_oth = (school_type!=1) if !missing(school_type)
* type_other includes special education, vocational, other/alternaticve and program(new since 2008).
*/

* High grade, Low grade
egen highm=mode(highgrade), by(bcode)
egen lowm=mode(lowgrade), by(bcode)
tab lowm highm, m
gen highschool=(lowm>=9) if !missing(lowm)
drop highgrade lowgrade

* There is a meang8_score variable but it was only created for the g8score quartiles subgroup. Create a new average of standardized g8 math and reading scores, g8std.
egen g8std=rmean(mathg8 readingg8)

* ACT/New SAT Standardized Scores
gen act_sat_std=actstd if year<2017
replace act_sat_std=satstd if year>=2017

* Number of schools in district
egen nsch_in_dcode=count(bcode), by(dcode year)
replace nsch_in_dcode= 0 if dcode==.
replace nsch_in_dcode=. if charter==1
replace nsch_in_dcode =. if year ==.

* Generate dummy vars if there's 1, 2, 3-9 and >10 schools in a district
gen nsch_in_dcode1=nsch_in_dcode==1 if !missing(nsch_in_dcode)
gen nsch_in_dcode2=nsch_in_dcode==2 if !missing(nsch_in_dcode)
gen nsch_in_dcode3_9=nsch_in_dcode>2 & nsch_in_dcode<10 if !missing(nsch_in_dcode)
gen nsch_in_dcode10=nsch_in_dcode>=10 if !missing(nsch_in_dcode)


* The outcomes we are interested in are stored in the following globals:
global enroll anyenr_1year_4year anyenr_1year_2year anyenr_1year
global insttype anyenr_1year_pub4out anyenr_1year_pub4in anyenr_1year_pub2out anyenr_1year_pub2in anyenr_1year_priv4in anyenr_1year_priv4out anyenr_1year_priv2out anyenr_1year_priv2in
global match match_col reach_col safety_col dist25_col_pct  dist75_col_pct   
global demo female black hisp asianamer poor sped lep act_sat_std g8std nstu
global school urb_city urb_rural urb_town highschool charter magnet nstu nsch_in_dcode1 nsch_in_dcode2 nsch_in_dcode3_9 nsch_in_dcode10
global id bcode year firstyear fte program_clean nsch_in_dcode school_type g8score_q

putexcel set "$results\MCAN_summary_stats_tmp.xlsx", sh("Stats by Treat Cohort_tmp") modify

local ncol = 2
local col: word `ncol' of `c(ALPHA)'
local row = 3

foreach fy in 2015 2016 2017 2018 2019 . {
count if tag==1 & firstyear==`fy'
putexcel `col'`row' = `r(N)'
local ++ncol
local col: word `ncol' of `c(ALPHA)'
}

local row=`row'+3
local ncol = 2
local col: word `ncol' of `c(ALPHA)'
*UNWEIGHTED SCHOOL CHARACTERISTICS
foreach fy in 2015 2016 2017 2018 2019 . {
foreach s in $school {
sum `s' if tag==1 & firstyear==`fy', d
putexcel `col'`row' = `r(mean)'
local row=`row'+1
}
local ++ncol
local col: word `ncol' of `c(ALPHA)'
local row = 6
}

local row = 20
local ncol = 2
local col: word `ncol' of `c(ALPHA)'
*WEIGHTED SCHOOL CHARACTERISTICS
foreach fy in 2015 2016 2017 2018 2019 . {
foreach s in urb_city urb_rural urb_town highschool charter magnet {   
sum `s' if tag==1 & firstyear==`fy' [aw=nstu], d
	putexcel `col'`row' = `r(mean)'
	local row=`row'+1
}
local ++ncol
local col: word `ncol' of `c(ALPHA)'
local row = 20
}
  
local row= 29
local ncol = 2
local col: word `ncol' of `c(ALPHA)'
*WEIGHTED DEMOGRAPHICS
foreach fy in 2015 2016 2017 2018 2019 . {
foreach s in $demo {
sum `s' if firstyear==`fy' [aw=nstu], d
putexcel `col'`row' = `r(mean)'
local row=`row'+1
}
local ++ncol
local col: word `ncol' of `c(ALPHA)'
local row = 29
}
* WEIGHTED ENROLLMENT, DISTANCE AND MATCH OUTCOMES
local row= 41
local ncol = 2
local col: word `ncol' of `c(ALPHA)'

foreach fy in 2015 2016 2017 2018 2019 . {
	foreach x in anyenr_1year anyenr_1year_4year {
		forv y=2010/2018 {
		sum `x' if firstyear==`fy' & year==`y' [aw=nstu], d
		putexcel `col'`row' = `r(mean)'
		local row=`row'+1
		}
	local row=`row'+2
	}
		forv y=2010/2017 {
		sum dist25_col_pct if firstyear==`fy' & year==`y' [aw=nstu], d
		putexcel `col'`row' = `r(mean)'
		local row=`row'+1
		}		
	local row=`row'+2
		forv y=2010/2016 {
		sum safety_col if firstyear==`fy' & year==`y' [aw=nstu], d
		putexcel `col'`row' = `r(mean)'
		local row=`row'+1
		}
local ++ncol
local col: word `ncol' of `c(ALPHA)'
local row = 41
}

