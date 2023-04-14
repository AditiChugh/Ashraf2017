********************************************************************************
* Traditional beliefs and learning about maternal risk in Zambia
* zambia - analysis
********************************************************************************

	clear all
	set more off

* define globals 
* change $zamb path to run in self-contained way 
	global zamb "C:\Users\gfr6\Dropbox\Zambia\pp\Field"
	global data "$zamb\Field_Data"
	global dofiles "$zamb\Field_Dofiles"
	global lat "$zamb\Field_Tables"
		
*********************************************************************************************
* use baseline data 
	use "$data\Field_baseline_all.dta", clear
		
*********************************************************************************************
* Descriptive Statistics
* Table 1
* Appendix Table A1
*********************************************************************************************

* variables for which ttest by gender is performed
	local outc 	worried_comp_likely recov_24_m kids_less4 age_less40  /// 
				exp_comp comp_fam died_fam comp_friend died_friend buttons_infidelity risk_comp_infidelity_b ///
				age edu_highest ///
				kids_opt tot_kids opt_space_m /// 
				cct_bad_w_health cct_decr_abil cct_use_unfaithful_b pillbadforhealth_b ///
				
* loop to perfom ttest by gender 
	foreach var in `outc' {
	estpost ttest `var', by(gender) 
	est store `var'_1
	
	* export each variable in a separate tex file
	estout `var'_1 ///
	using "$lat\ttest_`var'_all.tex", label replace ///
	cells(" mu_2(fmt(3)) mu_1(fmt(3)) se(par fmt(3) star)") style(tex) ///
	mlabels(, none) collabels(, none) ///
	eqlabels(, none) starlevels(* 0.10 ** 0.05 *** 0.01)
	}
*/
			
* use different rounding when exporting the salary variable
	estpost ttest mpay, by(gender) 
	est store mpay_1
	
	estout mpay_1 ///
	using "$lat\ttest_mpay_all.tex", label replace ///
	cells(" mu_2(fmt(1)) mu_1(fmt(1)) se(par fmt(1) star)") style(tex) ///
	mlabels(, none) collabels(, none) ///
	eqlabels(, none) starlevels(* 0.10 ** 0.05 *** 0.01)	
	
* export number of observations
	estpost ttest worried_comp_likely, by(gender) 
	est store obs_1
	
	estout obs_1 ///
	using "$lat\ttest_obs_all.tex", label replace ///
	cells(" N_2(fmt(0)) N_1(fmt(0)) . ") style(tex) ///
	mlabels(, none) collabels(, none) varlabel(worried_comp_likely "Observations") ///
	eqlabels(, none) starlevels(* 0.10 ** 0.05 *** 0.01)
		
		
**********************************************************************************************
* Regression Analysis
* Table 2 
**********************************************************************************************

* local for outcome variables
	local depen worried_comp_likely 
	
* covariates		
	local cov_1_hh	exp_comp  
	local cov_2_hh 	exp_comp_in risk_comp_infidelity_b  					
	local cov_all_hh exp_comp exp_comp_in risk_comp_infidelity_b  
					
* Loop to run a linear regression for both dependent variables
foreach var in `depen' {
	* women sample
		reg `var' `cov_1_hh'  				if gender == 1
		est store `var'_2
		reg `var' `cov_1_hh'  `cov_2_hh' 	if gender == 1
		est store `var'_3
	* men sample	
		reg `var' `cov_1_hh'  				if gender == 0
		est store `var'_4
		reg `var' `cov_1_hh' `cov_2_hh' 	if gender == 0
		est store `var'_5	
		
	* Export as separate tex file
			estout `var'_2 `var'_3 `var'_4 `var'_5  ///
			using "$lat\reg_`var'_all.tex", label replace ///
			cells(b(star fmt(3)) se(par fmt(3))) style(tex) ///
			mlabels(, none) collabels(, none) varlabels(_cons cons) ///
			eqlabels(, none) starlevels(* 0.10 ** 0.05 *** 0.01) ///
			keep(`cov_all_hh' _cons) ///
			order(`cov_all_hh' _cons) ///
			stats(N, fmt(0) )		
			est drop *			
		
}
*/		
	
*********************************************************************************************
* Infidelity weight - allocation by gender
* Figure 1
*********************************************************************************************
		
* Loop by gender - generate the bars in Figure 1 as percentage of buttons allocated to each category
	forval j = 0/1 {
	tab butt_inf if gender == `j', gen(butt_inf_`j')
	forval i = 1/7 {
	egen butt_inf_u_`j'`i' = mean(butt_inf_`j'`i') if butt_inf_`j'`i'!=. & gender == `j'
	replace butt_inf_u_`j'`i' = butt_inf_u_`j'`i' * 100	 // 0 - 100 scale
	}
	}
*	
* Determine placement of bars	
	forval i = 1/7 {
	local m: word `i' of 1 3.25 5.5 7.75 10 12.25 14.5
	gen index_u_1`i' = `m'
	gen index_u_0`i' = `m' + 1
	}
*	
	
* construct figure with bars indicating the percentage of buttons placed in a given category for women and men separately
* export graph as .png file
	twoway 	(bar butt_inf_u_11 index_u_11, bfcolor(gs4)  blcolor(gs4*2)  lwidth(0.4)) 	///
			(bar butt_inf_u_01 index_u_01, bfcolor(gs14) blcolor(gs14*2) lwidth(0.4)) 	///
			(bar butt_inf_u_12 index_u_12, bfcolor(gs4)  blcolor(gs4*2)  lwidth(0.4)) 	///
			(bar butt_inf_u_02 index_u_02, bfcolor(gs14) blcolor(gs14*2) lwidth(0.4)) 	///
			(bar butt_inf_u_13 index_u_13, bfcolor(gs4)  blcolor(gs4*2)  lwidth(0.4)) 	///
			(bar butt_inf_u_03 index_u_03, bfcolor(gs14) blcolor(gs14*2) lwidth(0.4)) 	///
			(bar butt_inf_u_14 index_u_14, bfcolor(gs4)  blcolor(gs4*2)  lwidth(0.4)) 	///
			(bar butt_inf_u_04 index_u_04, bfcolor(gs14) blcolor(gs14*2) lwidth(0.4)) 	///
			(bar butt_inf_u_15 index_u_15, bfcolor(gs4)  blcolor(gs4*2)  lwidth(0.4)) 	///
			(bar butt_inf_u_05 index_u_05, bfcolor(gs14) blcolor(gs14*2) lwidth(0.4)) 	///
			(bar butt_inf_u_16 index_u_16, bfcolor(gs4)  blcolor(gs4*2)  lwidth(0.4)) 	///
			(bar butt_inf_u_06 index_u_06, bfcolor(gs14) blcolor(gs14*2) lwidth(0.4)) 	///
			(bar butt_inf_u_17 index_u_17, bfcolor(gs4)  blcolor(gs4*2)  lwidth(0.4)) 	///
			(bar butt_inf_u_07 index_u_07, bfcolor(gs14) blcolor(gs14*2) lwidth(0.4)) ,	///
			graphregion(color(white) lwidth(large)) 									/// 
			yscale(r(0(10)50)) 	ylabel(0(10)50, glcolor(gs15) labsize(4)) ytitle("Percent", size(5))									///
			xscale(r(1(1)16)) 	xlabel( 1.25 "0" 3.75 "1-5" 6 "6-10" 8.25 "11-15" 10.5 "16-20" 12.75 "21-25" 14.75"26-30" , labsize(4)) ///
			legend( label(1 "Women") label(2 "Men") order(1 2) cols(2) symxsize(10) size(5) textwidth(15) region(lcolor(white)) ) 		///
			title( " ", size(3) color(black)) 											///
			name(butt_inf, replace) 
			graph export "$lat\figure_1.pdf", replace	

			drop  butt_inf_*  index_u_* 
		
*********************************************************************************************	
* Descriptive Statistics - couples only
* Appendix Table A2
*********************************************************************************************	

* variables for which ttest by gender is performed
	local outc 	age edu_highest   /// 
				worried_comp_likely recov_24_m kids_less4 age_less40  /// 
				buttons_infidelity risk_comp_infidelity_b ///
				exp_comp comp_fam died_fam comp_friend died_friend ///
				kids_opt tot_kids opt_space_m /// 
				cct_bad_w_health cct_decr_abil cct_use_unfaithful_b pillbadforhealth_b ///
				
* loop to perfom ttest by gender 
	foreach var in `outc' {
	estpost ttest `var' if couple == 1, by(gender) 
	est store `var'_1
	
	* export each variable in a separate tex file
	estout `var'_1 ///
	using "$lat\ttest_`var'_mar.tex", label replace ///
	cells(" mu_2(fmt(3)) mu_1(fmt(3)) se(par fmt(3) star)") style(tex) ///
	mlabels(, none) collabels(, none) ///
	eqlabels(, none) starlevels(* 0.10 ** 0.05 *** 0.01)
	}
*/			
	
* use different rounding to export the salary variable
	estpost ttest mpay if couple == 1, by(gender) 
	est store mpay_1
	
	estout mpay_1 ///
	using "$lat\ttest_mpay_mar.tex", label replace ///
	cells(" mu_2(fmt(1)) mu_1(fmt(1)) se(par fmt(1) star)") style(tex) ///
	mlabels(, none) collabels(, none) ///
	eqlabels(, none) starlevels(* 0.10 ** 0.05 *** 0.01)
	
* export number of observations				
	gen one = 1
	estpost ttest one if couple == 1, by(gender) 
	est store obs_1
	
	estout obs_1 ///
	using "$lat\ttest_obs_mar.tex", label replace ///
	cells(" N_2(fmt(0)) N_1(fmt(0)) . ") style(tex) ///
	mlabels(, none) collabels(, none) varlabel(one "Observations") ///
	eqlabels(, none) starlevels(* 0.10 ** 0.05 *** 0.01)	
		


