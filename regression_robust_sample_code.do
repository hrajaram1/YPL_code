/*******************************************************************************
Purpose: Robust regression analysis for outcomes any_enrollment and enrollment in 4 year college. Results output to 
"$results/MCAN_summary_and_analysis_results.xlsx", sh("Robust").

Created on: 1/13/2020
By: Hersheena Rajaram

Last modified on: 11/13/2020
By: Hersheena Rajaram
    
*******************************************************************************/

clear all
set more off
macro drop _all
cap log close
program drop _all
matrix drop _all
*set trace on
*set tracedepth 1

global date = c(current_date)
global username = c(username)

** File paths
global base "Y:\projects\MCAN"
global epi_base "O:\data_final"
global mcan_data "$base\data_final"
global temp_data "$base\data_temp"
global mcan_raw "$base\data_raw"
global results "$base\results"
global mcan_log "$base\code_logs"
global docu "$base\documentation"

*Load data
use "$mcan_data\pre_analysis_student_level.dta", clear

* Create a log file
log using "$mcan_log\regression_robust_${username}_${date}.log", replace 

*As a reminder, here are all our globals:
*student controls
global stu_covs "female black hisp asianamer poor sped lep mathg8 readingg8 mathg8_miss readingg8_miss poor_miss sped_miss lep_miss"

*neighborhood controls - basic on geocoded addresses
global ngh_covs ""

global sch_covs "school_nstu school_nstu_sq school_black school_hisp school_poor school_sped school_mathg8" 
global sch_fix "charter magnet city suburb town rural altconfig altsch vocsch"

global trt1 "t_m7 t_m6 t_m5 t_m4 t_m3 t_m2 t_m1      t_1 t_2 t_3 "
global yr1  "y_2011 y_2012 y_2013 y_2014 y_2015 y_2016 y_2017 y_2018"

global trt2 "     t_m5 t_m4 t_m3 t_m2 t_m1      t_1 t_2 t_3 "
global yr2  "       y_2012 y_2013 y_2014 y_2015 y_2016 y_2017 y_2018"

global trt3 "t_m4b t_m3 t_m2 t_m1      t_1 t_2 t_3 "
global trt3c "t_m4b t_m3 t_m2 t_m1      t_1c t_2c t_3c "


/*******************************************************************************
									Robust
*******************************************************************************/
* The first part of the code will just do the regressions and part 2 will do the regressions and output the results in a spreadsheet using putexcel.  I'm writing a part 1 so that the regression commands don't get lost in the putexcel code.
*PART 1: Only regressions


global out  "anyenr_1year anyenr_1year_4year reach_col reach_match_col"

foreach y of global out { 

	disp "*********************************************************************"
	disp "**** MODELS 1 & 2 : CONTROLS ARE ALL NON-TREATED UNITS  *****"
	disp "*********************************************************************"
* Model 1
  areg `y' $trt1 $yr1 if treatment_school==1|control_1==1, a(bcode) cl(bcode)
  testparm t_m*
  local `y'_model1 = `r(p)'
  disp ``y'_model1'
 

* Model 2	
  areg `y' $trt1 $yr1 $stu_covs $sch_covs if treatment_school==1|control_1==1, a(bcode) cl(bcode)
	testparm t_m*
	local `y'_model2 = `r(p)'
	disp ``y'_model2'
	
	disp "*********************************************************************"
	disp "**** MODELS 3 & 4: CONTROLS ARE NON-TREATED TARGET UNITS  *****" 
	disp "*********************************************************************"
*Model 3	
  areg `y' $trt1 $yr1 if treatment_school==1|control_2==1, a(bcode) cl(bcode)
	testparm t_m*
  local `y'_model3 = `r(p)'
  disp ``y'_model3'
  
*Model 4
  areg `y' $trt1 $yr1 $stu_covs $sch_covs if treatment_school==1|control_2==1, a(bcode) cl(bcode)
	testparm t_m*
  local `y'_model4 = `r(p)'
  disp ``y'_model4'
  	
	disp "*********************************************************************"
	disp "**** MODELS 5 & 6 : CONTROLS ARE UNITS TREATED OUTSIDE SAMPLE PERIOD *****"
	disp "*********************************************************************"
* Model 5
  areg `y' $trt1 $yr1 if treatment_school==1|control_3==1, a(bcode) cl(bcode)
  testparm t_m*
  local `y'_model5 = `r(p)'
  disp ``y'_model5'
  
* Model 6	
  areg `y' $trt1 $yr1 $stu_covs $sch_covs if treatment_school==1|control_3==1, a(bcode) cl(bcode)
	testparm t_m*
  local `y'_model6 = `r(p)'
  disp ``y'_model6'
	
	disp "**********************************************************************"
	disp "**** MODELS 7 & 8: CONTROLS ARE UNITS TREATED WITHIN SAMPLE PERIOD  *****"
	disp "*********************************************************************"
*Model 7	
  areg `y' $trt1 $yr2 if treatment_school==1, a(bcode) cl(bcode)
	testparm t_m*
  local `y'_model7 = `r(p)'
  disp ``y'_model7'
  
*Model 8	
  areg `y' $trt1 $yr2 $stu_covs $sch_covs if treatment_school==1, a(bcode) cl(bcode)
	testparm t_m*
  local `y'_model8 = `r(p)'
  disp ``y'_model8'

global `y'_pvalue ``y'_model1' ``y'_model2' ``y'_model3' ``y'_model4' ``y'_model5'` `y'_model6'` `y'_model7' ``y'_model8'
}

putexcel set "$results\MCAN_summary_and_analysis_11132020.xlsx", sh("t_tests") modify
local ncol=2
local col:word `ncol' of `c(ALPHA)'
local row = 4
foreach y of global out {
	foreach p in $`y'_pvalue {
	putexcel `col'`row' = `p'
	local ++ncol
	local col:word `ncol' of `c(ALPHA)'
	}
local row = `row'+1	
local ncol=2
local col:word `ncol' of `c(ALPHA)'
}


******************************************************************************************************************************************
* Part 2: Output results
putexcel set "$results\MCAN_summary_and_analysis_11132020.xlsx", sh("Robust") modify
global out  "anyenr_1year anyenr_1year_4year reach_col reach_match_col"

* First, calclualte and export the mean enrollment rate(any, 4-year) for the control group sample as per each model. Recall that 
/*																						      Model1  Model2  Model3  Model4  Model7  Model8 Model5	Model6
Control group is all schools that never particpated in MSU/AdviseMI or in another program		y		y		n		n		n		n		n		n
Control group is only future treated AdviseMI schools (2019-2020 cohorts)						n		n		y		y		n		n		n		n
Sample limited to schools that received treatment (2016-2018)									n		n		n		n		y		y		n		n
Control group is target schools (by MCAN's definition) that were never in any advising 			n		n		n		n		n		n		y		y
program before 2019
*/

local row = 14
local ncol=2
local col:word `ncol' of `c(ALPHA)'

foreach y of global out {
	foreach i in 1 1 2 2 3 3 {
	sum `y' if control_`i'==1,d
		putexcel `col'`row' = `r(mean)'
		local ncol = `ncol'+1
		local col: word `ncol' of `c(ALPHA)'
	
	}
	sum `y' if treatment_school==1,d
		putexcel `col'`row' = `r(mean)'
		local ncol = `ncol'+1
		local col: word `ncol' of `c(ALPHA)'
		putexcel `col'`row' = `r(mean)'
local row = `row'+22
local ncol=2
local col: word `ncol' of `c(ALPHA)'
}

* Next, output the number of treatment schools and control schools for each model for outcomes any enroll and 4 year enroll.
foreach i in 1 1 2 2 3 3 {
	local row = 22
count if treatment_school==1 & tagsch==1  //
	putexcel `col'`row' = `r(N)'
		local row = `row'+1
count if control_`i'==1 & tagsch==1	
	putexcel `col'`row' = `r(N)'
		local ++ncol
		local col: word `ncol' of `c(ALPHA)'
}	
* Now, let's output the regression results
local row = 5
foreach y of global out { 
local ncol = 2
local col: word `ncol' of `c(ALPHA)'

	disp "*********************************************************************"
	disp "**** MODELS 1 & 2 : CONTROLS ARE ALL NON-TREATED UNITS  *****"
	disp "*********************************************************************"
/************************************* MODEL 1 *********************************/
  areg `y' $trt1 $yr1 if treatment_school==1|control_1==1, a(bcode) cl(bcode)
  forv i = 1(1)3 {
	local r = `i'+7
    matrix tmp = r(table)
	local coef = tmp[1,`r']
	local coef_t`i' = round(`coef', 0.001)		//Coeff: AdviseMI Year `i'
	local ser = tmp[2,`r']
	local ser_t`i' = round(`ser', 0.001)		//Standard error: AdviseMI Year `i'
	local p_t`i' = tmp[4,`r']
  if `p_t`i''<0.01 {
	local coef_t`i' = "`coef_t`i''"+"***"		// Set up signifcance stars for 1%(***), 5%(**) and 10%(*) significance levels
	}
  else if `p_t`i''<0.05 {
	local coef_t`i' = "`coef_t`i''"+"**"
	}
  else if `p_t`i''<0.10 {
	local coef_t`i' = "`coef_t`i''"+"*"
	}
  putexcel `col'`row' = "`coef_t`i''"
	local row=`row'+1
  putexcel `col'`row' = "(`ser_t`i'')"
	local row = `row'+2  
  }
local row = `row'+  7
putexcel `col'`row' = `e(N)'
local row = `row'+6
}

/************************************* MODEL 2 *********************************/
local row = 5
foreach y of global out {
local ncol = 3
local col: word `ncol' of `c(ALPHA)'

  areg `y' $trt1 $yr1 $stu_covs $sch_covs if treatment_school==1|control_1==1, a(bcode) cl(bcode)
	forv i = 1(1)3 {
		local r = `i'+7
		matrix tmp = r(table)
		local coef = tmp[1,`r']
		local coef_t`i' = round(`coef', 0.001)		//Coeff: AdviseMI Year `i'
		local ser = tmp[2,`r']
		local ser_t`i' = round(`ser', 0.001)		//Standard error: AdviseMI Year `i'
		local p_t`i' = tmp[4,`r']
	if `p_t`i''<0.01 {
		local coef_t`i' = "`coef_t`i''"+"***"		// Set up signifcance stars for 1%(***), 5%(**) and 10%(*) significance levels
		}
	else if `p_t`i''<0.05 {
		local coef_t`i' = "`coef_t`i''"+"**"
		}
	else if `p_t`i''<0.10 {
		local coef_t`i' = "`coef_t`i''"+"*"
		}
	putexcel `col'`row' = "`coef_t`i''"
		local row=`row'+1
	putexcel `col'`row' = "(`ser_t`i'')"
		local row = `row'+2  
	}
local row = `row'+  7
putexcel `col'`row' = `e(N)'
local row = `row'+6
}
	
  
local row = 5
foreach y of global out { 
local ncol = 4
local col: word `ncol' of `c(ALPHA)'

	disp "*********************************************************************"
	disp "**** MODELS 3 & 4: CONTROLS ARE NON-TREATED TARGET UNITS *****"
	disp "*********************************************************************"
	
/************************************* MODEL 3 *********************************/
  areg `y' $trt1 $yr1 if treatment_school==1|control_2==1, a(bcode) cl(bcode)
  forv i = 1(1)3 {
	local r = `i'+7
    matrix tmp = r(table)
	local coef = tmp[1,`r']
	local coef_t`i' = round(`coef', 0.001)		//Coeff: AdviseMI Year `i'
	local ser = tmp[2,`r']
	local ser_t`i' = round(`ser', 0.001)		//Standard error: AdviseMI Year `i'
	local p_t`i' = tmp[4,`r']
  if `p_t`i''<0.01 {
	local coef_t`i' = "`coef_t`i''"+"***"		// Set up signifcance stars for 1%(***), 5%(**) and 10%(*) significance levels
	}
  else if `p_t`i''<0.05 {
	local coef_t`i' = "`coef_t`i''"+"**"
	}
  else if `p_t`i''<0.10 {
	local coef_t`i' = "`coef_t`i''"+"*"
	}
  putexcel `col'`row' = "`coef_t`i''"
	local row=`row'+1
  putexcel `col'`row' = "(`ser_t`i'')"
	local row = `row'+2  
  }
local row = `row'+  7
putexcel `col'`row' = `e(N)'
local row = `row'+6
}

local row = 5
/************************************* MODEL 4 *********************************/
foreach y of global out {
local ncol = 5
local col: word `ncol' of `c(ALPHA)'

  areg `y' $trt1 $yr1 $stu_covs $sch_covs if treatment_school==1|control_2==1, a(bcode) cl(bcode)
	forv i = 1(1)3 {
		local r = `i'+7
		matrix tmp = r(table)
		local coef = tmp[1,`r']
		local coef_t`i' = round(`coef', 0.001)		//Coeff: AdviseMI Year `i'
		local ser = tmp[2,`r']
		local ser_t`i' = round(`ser', 0.001)		//Standard error: AdviseMI Year `i'
		local p_t`i' = tmp[4,`r']
	if `p_t`i''<0.01 {
		local coef_t`i' = "`coef_t`i''"+"***"		// Set up signifcance stars for 1%(***), 5%(**) and 10%(*) significance levels
		}
	else if `p_t`i''<0.05 {
		local coef_t`i' = "`coef_t`i''"+"**"
		}
	else if `p_t`i''<0.10 {
		local coef_t`i' = "`coef_t`i''"+"*"
		}
	putexcel `col'`row' = "`coef_t`i''"
		local row=`row'+1
	putexcel `col'`row' = "(`ser_t`i'')"
		local row = `row'+2  
	}
local row = `row'+  7
putexcel `col'`row' = `e(N)'
local row = `row'+6
}


local row = 5
foreach y of global out { 
local ncol = 6
local col: word `ncol' of `c(ALPHA)'

	disp "**********************************************************************"
	disp "*** MODELS 5 & 6: CONTROLS ARE UNITS TREATED WITHIN SAMPLE PERIOD  ***"
	disp "*********************************************************************"
/************************************* MODEL 5 *********************************/
  areg `y' $trt1 $yr1 if treatment_school==1|control_3==1, a(bcode) cl(bcode)
  forv i = 1(1)3 {
	local r = `i'+7
    matrix tmp = r(table)
	local coef = tmp[1,`r']
	local coef_t`i' = round(`coef', 0.001)		//Coeff: AdviseMI Year `i'
	local ser = tmp[2,`r']
	local ser_t`i' = round(`ser', 0.001)		//Standard error: AdviseMI Year `i'
	local p_t`i' = tmp[4,`r']
  if `p_t`i''<0.01 {
	local coef_t`i' = "`coef_t`i''"+"***"		// Set up signifcance stars for 1%(***), 5%(**) and 10%(*) significance levels
	}
  else if `p_t`i''<0.05 {
	local coef_t`i' = "`coef_t`i''"+"**"
	}
  else if `p_t`i''<0.10 {
	local coef_t`i' = "`coef_t`i''"+"*"
	}
  putexcel `col'`row' = "`coef_t`i''"
	local row=`row'+1
  putexcel `col'`row' = "(`ser_t`i'')"
	local row = `row'+2  
  }
local row = `row'+  7
putexcel `col'`row' = `e(N)'
local row = `row'+6
}

/************************************* MODEL 6 *********************************/
local row = 5
foreach y of global out {
local ncol = 7
local col: word `ncol' of `c(ALPHA)'

  areg `y' $trt1 $yr1 $stu_covs $sch_covs if treatment_school==1|control_3==1, a(bcode) cl(bcode)
	forv i = 1(1)3 {
		local r = `i'+7
		matrix tmp = r(table)
		local coef = tmp[1,`r']
		local coef_t`i' = round(`coef', 0.001)		//Coeff: AdviseMI Year `i'
		local ser = tmp[2,`r']
		local ser_t`i' = round(`ser', 0.001)		//Standard error: AdviseMI Year `i'
		local p_t`i' = tmp[4,`r']
	if `p_t`i''<0.01 {
		local coef_t`i' = "`coef_t`i''"+"***"		// Set up signifcance stars for 1%(***), 5%(**) and 10%(*) significance levels
		}
	else if `p_t`i''<0.05 {
		local coef_t`i' = "`coef_t`i''"+"**"
		}
	else if `p_t`i''<0.10 {
		local coef_t`i' = "`coef_t`i''"+"*"
		}
	putexcel `col'`row' = "`coef_t`i''"
		local row=`row'+1
	putexcel `col'`row' = "(`ser_t`i'')"
		local row = `row'+2  
	}
local row = `row'+  7
putexcel `col'`row' = `e(N)'
local row = `row'+6
}


local row = 5
foreach y of global out { 
local ncol = 8
local col: word `ncol' of `c(ALPHA)'

	disp "**********************************************************************"
	disp "**** MODELS 7 & 8: CONTROLS ARE UNITS TREATED WITHIN SAMPLE PERIOD  *****"
	disp "*********************************************************************"
/************************************* MODEL 7 *********************************/
  areg `y' $trt1 $yr2 if treatment_school==1, a(bcode) cl(bcode)
  forv i = 1(1)3 {
	local r = `i'+7
    matrix tmp = r(table)
	local coef = tmp[1,`r']
	local coef_t`i' = round(`coef', 0.001)		//Coeff: AdviseMI Year `i'
	local ser = tmp[2,`r']
	local ser_t`i' = round(`ser', 0.001)		//Standard error: AdviseMI Year `i'
	local p_t`i' = tmp[4,`r']
  if `p_t`i''<0.01 {
	local coef_t`i' = "`coef_t`i''"+"***"		// Set up signifcance stars for 1%(***), 5%(**) and 10%(*) significance levels
	}
  else if `p_t`i''<0.05 {
	local coef_t`i' = "`coef_t`i''"+"**"
	}
  else if `p_t`i''<0.10 {
	local coef_t`i' = "`coef_t`i''"+"*"
	}
  putexcel `col'`row' = "`coef_t`i''"
	local row=`row'+1
  putexcel `col'`row' = "(`ser_t`i'')"
	local row = `row'+2  
  }
local row = `row'+  7
putexcel `col'`row' = `e(N)'
local row = `row'+6
}

/************************************* MODEL 8 *********************************/
local row = 5
foreach y of global out {
local ncol = 9
local col: word `ncol' of `c(ALPHA)'

  areg `y' $trt1 $yr2 $stu_covs $sch_covs if treatment_school==1, a(bcode) cl(bcode)
	forv i = 1(1)3 {
		local r = `i'+7
		matrix tmp = r(table)
		local coef = tmp[1,`r']
		local coef_t`i' = round(`coef', 0.001)		//Coeff: AdviseMI Year `i'
		local ser = tmp[2,`r']
		local ser_t`i' = round(`ser', 0.001)		//Standard error: AdviseMI Year `i'
		local p_t`i' = tmp[4,`r']
	if `p_t`i''<0.01 {
		local coef_t`i' = "`coef_t`i''"+"***"		// Set up signifcance stars for 1%(***), 5%(**) and 10%(*) significance levels
		}
	else if `p_t`i''<0.05 {
		local coef_t`i' = "`coef_t`i''"+"**"
		}
	else if `p_t`i''<0.10 {
		local coef_t`i' = "`coef_t`i''"+"*"
		}
	putexcel `col'`row' = "`coef_t`i''"
		local row=`row'+1
	putexcel `col'`row' = "(`ser_t`i'')"
		local row = `row'+2  
	}
local row = `row'+  7
putexcel `col'`row' = `e(N)'
local row = `row'+6
}


* Next, output the number of treatment schools and control schools for each model for outcomes reach and reach/match college
drop if reach_col==.
sort bcode year
egen  tag_reach = tag(bcode)

local ncol = 2
local col: word `ncol' of `c(ALPHA)'

foreach i in 1 1 2 2 3 3 {
	local row = 66
count if treatment_school==1 & tag_reach==1  //
	putexcel `col'`row' = `r(N)'
		local row = `row'+1
count if control_`i'==1 & tag_reach==1	
	putexcel `col'`row' = `r(N)'
		local ++ncol
		local col: word `ncol' of `c(ALPHA)'
}	

log close
/*	