********************************************************************************
********************************************************************************
******************************  Analysis for  **********************************
************  "Political Ideology and Generosity Around the Globe"  ************
********************************************************************************
************* by V. Capraro, X. Demaj, R. Di Paolo and V. Pizziol  *************
********************************************************************************
********************************************************************************

**Prepare environment in Stata
clear all
set more off		

**Set directory
cd "..."

**Load dataset
use dataset.dta, clear


********************************
********  Correlations analysis
********************************
pwcorr political_ideology selfinterest national international nid cnarc ind_narcis mor_circle moral_id moral_coope open_mind health_cond slf_ladder riskperc1 riskperc2 consp_beliefs gender age employed student, sig star(.05)

*******************************
********  Regressions analysis
*******************************
reg selfinterest political_ideology, vce(robust)
est sto model1
reg national political_ideology, vce(robust)
est sto model2
reg international political_ideology, vce(robust)
est sto model3
reghdfe selfinterest political_ideology, vce(robust) absorb(iso)
est sto model4
reghdfe national political_ideology, vce(robust) absorb(iso)
est sto model5
reghdfe international political_ideology, vce(robust) absorb(iso)
est sto model6
reghdfe selfinterest political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student, vce(robust) absorb(iso)
est sto model7
reghdfe national political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student, vce(robust) absorb(iso)
est sto model8
reghdfe international political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student, vce(robust) absorb(iso)
est sto model9

**Table 1
esttab model1 model2 model3 model4 model5 model6 model7 model8 model9 using regressions.tex, c(b(fmt(3) star) se(fmt(3) par)) starlevels(* 0.05 ** 0.01 *** 0.001) r2 replace

******************************
********  Mixed-effect models
******************************

**Generate WGI Index
gen wgi = (gov_effectiveness + reg_quality + voice_acountability + ruleoflaw + corruption + pol_stab_noviolence)/6

mixed selfinterest c.political_ideology##c.wgi || iso:
summarize political_ideology 
return list
margins, at(wgi =(-2.1(.5)1.8) political_ideology=(0.1 0.9))
**Figure 1 panel a
marginsplot, byopts(name(graph1)) recastci(rarea) ciopts(color(none) lcolor(black))

mixed national c.political_ideology##c.wgi || iso:
summarize political_ideology 
return list
margins, at(wgi =(-2.1(.5)1.8) political_ideology=(0.1 0.9))
**Figure 1 panel b
marginsplot, byopts(name(graph2)) recastci(rarea) ciopts(color(none) lcolor(black))

mixed international c.political_ideology##c.wgi || iso:
summarize political_ideology 
return list
margins, at(wgi =(-2.1(.5)1.8) political_ideology=(0.1 0.9))
**Figure 1 panel c
marginsplot, byopts(name(graph3)) recastci(rarea) ciopts(color(none) lcolor(black))



****************************
**** Analysis after Revision
****************************

*** Descriptive Statistics (Table S1)

label variable political_ideology "Political Ideology"
label variable	gender "Gender"
label variable age "Age"
label variable employed "Employment Status" 
label variable student "Student Status" 
label variable covid_cases "Covid-19 cases"

eststo stat: estpost tabstat political_ideology gender age employed student covid_cases , by(iso) s(mean sd N) nototal
esttab stat using "./stat.tex", cells("political_ideology(fmt(3)) gender(fmt(3)) age(fmt(3)) employed(fmt(3)) student(fmt(3)) covid_cases(fmt(3))") label noobs collabels("Political Ideology" "Gender" "Age" "Employment Status" "Student Status" "Covid-19 cases") nonote booktabs f nonumber replace


*** Robustness check controlling for the serverity of the pandemic (Table S2)

mixed selfinterest  political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student covid_cases || iso: 
eststo mix1
mixed national  political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student covid_cases || iso: 
eststo mix2
mixed international  political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student covid_cases || iso: 
eststo mix3

esttab mix1 mix2 mix3 using robust.tex, c(b(fmt(3) star) se(fmt(3) par)) starlevels(* 0.05 ** 0.01 *** 0.001) r2 replace


** Robustness check: representative vs convenient sample (Table S3)

reghdfe selfinterest  political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student if revision_coding==1, vce(robust) absorb(iso)
reghdfe selfinterest  political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student if revision_coding==0, vce(robust) absorb(iso)

reghdfe national  political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student if revision_coding==1, vce(robust) absorb(iso)
reghdfe national  political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student if revision_coding==0, vce(robust) absorb(iso)

reghdfe international  political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student if revision_coding==1, vce(robust) absorb(iso)
reghdfe international  political_ideology cnarc nid ind_narcis mor_circle moral_coope open_mind health_cond slf_ladder consp_beliefs gender age employed student if revision_coding==0, vce(robust) absorb(iso)
