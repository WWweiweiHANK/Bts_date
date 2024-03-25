
##########################################################################################################
## This document will reproduce the analysis of the "The globalizability of temporal discounting". 
## Author 1, Author2, ...
## Submitted for publication to Nature Human Behavior
## This document is preregistered at OSF. 
## The following functions are necessary to handle data wrangling as well as to compute the different scores.
##########################################################################################################


# This series of scripts will reproduce the analysis of the "The globalizability of temporal discounting". This document is preregistered at OSF. 
## This script will estimate all main models, including gam, hierarchical gam, hierarchical linear & logistic models, and the respective Bayesian versions of these models.

########################
# 0. Utility functions
########################

source("999_1_auxiliary_functions.R")

########################
# 1. GAMM estimation
########################

if (!require("mgcv")) install.packages("mgcv")
if (!require("gratia")) install.packages("gratia")
if (!require("itsadug")) install.packages("itsadug")
if (!require("htmlTable")) install.packages("htmlTable")
if (!require("job")) install.packages("job")
if (!require("sjPlot")) install.packages("sjPlot")

load("1_2_data_files.RData")


## Estimation of effects for temporal discount scores 
### Details of the estimation are provided in Supplementary materials.

job::job({
### We employ the job package to free the console during the estimation of these models.
  results_scores_bam <- dat_filter %>% 
    select(score.gmcs, 
           Residence, 
           ResponseId, 
           Gender, 
           EducationCompleted, 
           indineq.cwc,
           Employment, 
           age.cwc, 
           GDP.gmc, 
           GDP.log, 
           GINI.gmc, 
           inflation.gmc,
           debt.cwc ,
           assets.cwc ,
           
    ) %>% 
    bam_estimation(., bs = "cs", family = "gaussian")
}, import = c (bam_estimation, dat_filter))

## Estimation of effects for each anomaly.
### We employ the job package to free the console during the estimation of these models.
### Details of the estimation are provided in Supplementary materials.

dat_anoms <- list()
vars <- c("presbias", "absolmag", "gainloss", "delayspeed", "subaddit")
for (i in 1:length(vars)) {
  dat_anoms[[i]] <- dat_filter %>% 
    select(vars[i], 
           Residence, 
           ResponseId, 
           Gender, 
           EducationCompleted, 
           indineq.cwc,
           Employment, 
           age.cwc, 
           GDP.gmc, 
           GINI.gmc, 
           inflation.gmc,
           debt.cwc ,
           assets.cwc)
}

job::job ({
  results_anomalies_bam <- map(dat_anoms, bam_estimation, bs = "cs", family = "binomial")
}, import = c (bam_estimation, dat_anoms))



## For interested readeres, we plot the summary of each  model. HTML verions of the same are displayed below.
score_gam <- htmlTable(gamtabs(results_scores_bam[[1]], caption='Summary of scores gamm', type = "HTML")); score_gam
pbias_gam <- htmlTable(gamtabs(results_anomalies_bam[[1]][[1]], caption='Summary of present bias gamm', type = "HTML")); pbias_gam
absol_gam <- htmlTable(gamtabs(results_anomalies_bam[[2]][[1]], caption='Summary of absolute magnitude gamm', type = "HTML")); absol_gam
gainloss_gam <- htmlTable(gamtabs(results_anomalies_bam[[3]][[1]], caption='Summary of gain-loss assymetry gamm', type = "HTML")); gainloss_gam
delay_gam <- htmlTable(gamtabs(results_anomalies_bam[[4]][[1]], caption='Summary of delay-speedup gamm', type = "HTML"));delay_gam
subad_gam <-htmlTable(gamtabs(results_anomalies_bam[[5]][[1]], caption='Summary of subadditivity gamm', type = "HTML")); subad_gam


########################################################################
# 2. Hierarchical GAMM and linear/generalized model estimation
########################################################################

if (!require("lme4")) install.packages("lme4")
if (!require("lmerTest")) install.packages("lmerTest")
if (!require("emmeans")) install.packages("emmeans")
if (!require("gamm4")) install.packages("gamm4") 
if (!require("brms")) install.packages("brms") 
if (!require("cmdstanr")) install.packages("cmdstanr") 
if (!require("sjPlot")) install.packages("sjPlot")


## Estimation of temporal discount scores effects
### This routine will compute all models under comparison, and stored in table_res_scores

job::job({
  results_scores_lmer <- dat_filter %>% 
    select(score.gmcs, 
           Residence, 
           ResponseId, 
           Gender, 
           EducationCompleted, 
           indineq.cwc,
           Employment, 
           age.cwc, 
           GDP.gmc, 
           GINI.gmc, 
           inflation.gmc,
           debt.cwc ,
           assets.cwc) %>% 
    fit_models_lmer_ns(.)
}, import = c(dat_filter, fit_models_lmer_ns))

table_res_scores <- tab_model(results_scores_lmer$mg1$mer,show.icc = T); table_res_scores
score_lmer_gam <- htmlTable(gamtabs(results_scores_lmer$mg1$gam, caption='Summary of scores gamm', type = "HTML")); score_gam

## Estimation of effects for all anomalies
### This routine will compute all models under comparison, and stored in table_res_anom_lmer (results from lme4) and table_res_anom_gam (gamm4)

dat_anoms <- list()
vars <- c("presbias", "absolmag", "gainloss", "delayspeed", "subaddit")
for (i in 1:length(vars)) {
  dat_anoms[[i]] <- dat_filter %>% 
    select(vars[i], Residence, 
           ResponseId, 
           Gender, 
           EducationCompleted, 
           indineq.cwc,
           Employment, 
           age.cwc, 
           GDP.gmc, 
           GINI.gmc, 
           inflation.gmc,
           debt.cwc ,
           assets.cwc ) 
}

job::job ({
  results_anomalies_lmer <- map(dat_anoms, fit_models_glmer_ns)
}, import = c(dat_anoms, fit_models_glmer_ns))


table_res_anom_lmer <- list()
for (i in 1:length(results_anomalies_lmer)) {
  
  table_res_anom_lmer[[i]] <- tab_model(results_anomalies_lmer[[1]]$mg1$mer,
                                        results_anomalies_lmer[[2]]$mg1$mer,
                                        results_anomalies_lmer[[3]]$mg1$mer,
                                        results_anomalies_lmer[[4]]$mg1$mer,
                                        results_anomalies_lmer[[5]]$mg1$mer,
                                        transform = "exp",
                                        show.aic = T)
  }

pbias_lmer_gam <- htmlTable(gamtabs(results_anomalies_lmer[[1]]$mg1$gam, caption='Summary of present bias gamm', type = "HTML")); pbias_gam
absol_lmer_gam <- htmlTable(gamtabs(results_anomalies_lmer[[2]]$mg1$gam, caption='Summary of absolute magnitude gamm', type = "HTML")); absol_gam
gainloss_lmer_gam <- htmlTable(gamtabs(results_anomalies_lmer[[3]]$mg1$gam, caption='Summary of gain-loss assymetry gamm', type = "HTML")); gainloss_gam
delay_lmer_gam <- htmlTable(gamtabs(results_anomalies_lmer[[4]]$mg1$gam, caption='Summary of delay-speedup gamm', type = "HTML"));delay_gam
subad_lmer_gam <-htmlTable(gamtabs(results_anomalies_lmer[[5]]$mg1$gam, caption='Summary of subadditivity gamm', type = "HTML")); subad_gam

########################################################################
# 3. Bayesian linear/generalized model estimation
########################################################################

# This script will estimate all models employing Bayesian estimation techniques. 
## All models are estimated using the controls specified in the supplementary material, and with the inflation, debt and assets as non-linear terms for scores, and inflation and assets for the anomalies.
## Warning: It can take a few hours to complete. 

## Estimation of temporal discount scores effects
### This routine will compute all models under comparison, and stored in table_bayes_res_scores
job::job({
  results_scores_brm <- dat_filter %>% 
    select(score.gmcs, 
           Residence, 
           ResponseId, 
           Gender, 
           EducationCompleted, 
           indineq.cwc,
           Employment, 
           age.cwc, 
           GDP.gmc, 
           GINI.gmc, 
           inflation.gmc,
           debt.cwc ,
           assets.cwc) %>% 
    fit_models_brm_score_ns(., dist = "gaussian")
}, import = c(dat_filter, fit_models_brm_score_ns))


table_bayes_res_scores <- tab_model(results_scores_brm[[1]], show.icc = T); table_bayes_res_scores


## Estimation of effects for all anomalies
### This routine will compute all models under comparison, and stored in table_bayes_res_anom_lmer (results from lme4) and table_bayes_res_anom_gam (gamm4)

dat_anoms <- list()
vars <- c("presbias", "absolmag", "gainloss", "delayspeed", "subaddit")
for (i in 1:length(vars)) {
  dat_anoms[[i]] <- dat_filter %>% 
    select(vars[i],  score.gmcs, 
           Residence, 
           ResponseId, 
           Gender, 
           EducationCompleted, 
           indineq.cwc,
           Employment, 
           age.cwc, 
           GDP.gmc, 
           GINI.gmc, 
           inflation.gmc,
           debt.cwc ,
           assets.cwc) 
}

job::job({
  results_anomalies_brm <- map(dat_anoms, fit_models_brm_anomalies_ns, dist = "bernoulli")
}, import = c(dat_anoms, fit_models_brm_anomalies_ns))

table_bayes_res_anoms <- tab_model(results_anomalies_brm[[1]]$mg1,
                                   results_anomalies_brm[[2]]$mg1,
                                   results_anomalies_brm[[3]]$mg1,
                                   results_anomalies_brm[[4]]$mg1,
                                   results_anomalies_brm[[5]]$mg1,
                                   show.icc = T)


# Save all estimated models in the file 2_2_main_resuts.Rdata

save(
     results_scores_bam, 
     results_scores_lmer, 
     results_scores_brm, 
     results_anomalies_bam, 
     results_anomalies_lmer, 
     results_anomalies_brm, 
     file = "2_2_main_results_updated.Rdata")
