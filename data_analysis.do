
///////// Table 1 | Sample characteristics	   		  

foreach var of varlist district_1 grade gender_parents education_2 employment_1 income_1 maternal_vaccination fata_exp_1 perc_9_1_s perc_9_2_s perc_10_1_s perc_10_2_s perc_10_3_s perc_11_1_s perc_11_2_s perc_11_3_s perc_11_4_s perc_12_1_s perc_12_2_s perc_12_3_s perc_12_4_s {
  tab treatment `var' if time==0, chi2 row
}
foreach var of varlist age age_parents perc_s_10 conf_1 conf_2 willingness {
  ttest `var', by(treatment), if time==0
}

///////// Table 2 | Effects of a simulated fact-checking extension on parents' ability to evaluate HPV vaccine information

gen score1 = statements2_s + statements1_s + statements4_s
gen score2 = statements8_s + statements9_s + statements5_s
gen score3 = statements2_s + statements1_s + statements4_s + statements8_s + statements9_s + statements5_s
gen score4 = statements6_s + statements7_s
gen score5 = statements3_s + statements10_s
gen score6 = statements6_s + statements7_s + statements3_s + statements10_s
gen score7 = statements6_s + statements7_s + statements3_s + statements10_s + statements2_s + statements1_s + statements4_s + statements8_s + statements9_s + statements5_s

foreach var of varlist score1 score2 score3 score4 score5 score6 score7 {
  ttest `var' if treatment==1, by(time)
}
foreach var of varlist score1 score2 score3 score4 score5 score6 score7 {
  ttest `var' if treatment==0, by(time)
}

gen did = treatment*time
foreach var of varlist score1 score2 score3 score4 score5 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust)
}

* Calculate Cohen's d
* Based on pooled SD at baseline (time==0)

foreach var of varlist score3 score6 score7 {
    
    * Extract baseline SD for treatment group
    quietly summarize `var' if treatment==1 & time==0
    local sd1 = r(sd)
    
    * Extract baseline SD for control group
    quietly summarize `var' if treatment==0 & time==0
    local sd0 = r(sd)
    
    * Calculate pooled SD
    local pooled_sd = sqrt((`sd1'^2 + `sd0'^2) / 2)
    
    * Extract coefficient of did from GEE model
    quietly xtgee `var' treatment time did,family(gaussian) link(identity) corr(exchangeable) vce(robust)
    local coef = _b[did]
    
    * Calculate Cohen's d
    local cohens_d = `coef' / `pooled_sd'
    
    * Display results
    display "================================"
    display "Variable: `var'"
    display "Treatment baseline SD: " `sd1'
    display "Control baseline SD: " `sd0'
    display "Pooled SD: " `pooled_sd'
    display "Coefficient (did): " `coef'
    display "Cohen's d: " `cohens_d'
}

///////// Table 3 | Effects of a simulated fact-checking extension on parents' attitudes toward HPV vaccination

foreach var of varlist conf_1_1_new conf_2_1_new willingness_1_new {
  by treatment,sort:tab time `var', chi2 row
}

xtgee conf_1_1_new treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform

xtgee conf_2_1_new treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform

xtgee willingness_1_new treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform

///////// Table 4 | Participant attitude and adoption of the HPV vaccine fact-checking extension
gen exten_satisfication2 = 1 if exten_satisfication == 4 | exten_satisfication == 5
replace exten_satisfication2 = 0  if exten_satisfication2 == .

gen exten_confidence2 = 1 if exten_confidence == 4 | exten_confidence == 5
replace exten_confidence2 = 0  if exten_confidence2 == .

gen exten_willingness2 = 1 if exten_willingness == 4 | exten_willingness == 5
replace exten_willingness2 = 0  if exten_willingness2 == .

gen exten_recommen2 = 1 if exten_recommen == 4 | exten_recommen == 5
replace exten_recommen2 = 0  if exten_recommen2 == .

foreach var of varlist switch_3_1 exten_satisfication2 exten_confidence2 exten_willingness2 exten_recommen2 {
  tab treatment `var' if time==1, chi2 row
}

///////// Appendix Primary outcomes Module 1 | Module 2

foreach var of varlist statements1_s statements2_s statements3_s statements4_s statements5_s statements6_s statements7_s statements8_s statements9_s statements10_s {
  by treatment,sort:tab time `var', chi2 row
}

xtgee statements2_s treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform 
xtgee statements1_s treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform 
xtgee statements4_s treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform
xtgee statements8_s treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform
xtgee statements9_s treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform 
xtgee statements5_s treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform 

xtgee statements6_s treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform 
xtgee statements7_s treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform
xtgee statements3_s treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform 
xtgee statements10_s treatment time did, family(binomial) link(log) corr(exchangeable) vce(robust) eform 


///////// Subgroup analysis

foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust)
}

foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if district_1 == 1
}
foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if district_1 == 2
}
foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if district_1 == 3
}

foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if gender_parents == 0
}
foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if gender_parents == 1
}

foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if education_2 == 1
}
foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if education_2 == 2
}
foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if education_2 == 3
}

foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if employment_1 == 1
}
foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if employment_1 == 0
}

foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if income_1 == 1
}
foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if income_1 == 2
}
foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if income_1 == 3
}
foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if income_1 == 4
}

foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if maternal_vaccination == 1
}
foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if maternal_vaccination == 0
}

foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if fata_exp_1 == 1
}
foreach var of varlist score3 score6 score7 {
  xtgee `var' treatment time did, family(gaussian) link(identity) corr(exchangeable) vce(robust), if fata_exp_1 == 0
}

///////// Appendix Module 3 | Proportions of parents correctly answered HPV or its vaccine related statements on module 3.

by treatment2,sort:tab time statements11_s, chi2 row
xtgee statements11_s treatment2 time did2, family(binomial) link(log) corr(exchangeable) vce(robust) eform


