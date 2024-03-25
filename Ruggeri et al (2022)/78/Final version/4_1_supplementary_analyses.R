
##########################################################################################################
## This document will reproduce the analysis of the "The globalizability of temporal discounting". 
## Author 1, Author2, ...
## Submitted for publication to Nature Human Behavior
## This document is preregistered at OSF. 
## The following functions are necessary to handle data wrangling as well as to compute the different scores.
##########################################################################################################


# This series of scripts will reproduce the analysis of the "The globalizability of temporal discounting". This document is preregistered at OSF. 
## This script will provide all analyses presented in the supplementary data


########################
# 0. Uility functions
########################

source("999_1_auxiliary_functions.R")

########################
# 1. Figure S1
########################

load("1_2_data_files.RData")
load("2_2_main_results.RData")

if (!require("lme4")) install.packages("lme4")
if (!require("lmerTest")) install.packages("lmerTest")
if (!require("emmeans")) install.packages("emmeans")
if (!require("mgcv")) install.packages("mgcv")
if (!require("gamm4")) install.packages("gamm4")
if (!require("extrafont")) install.packages("extrafont")
if (!require("ggeffects")) install.packages("ggeffects")
if (!require("gratia")) install.packages("gratia")
if (!require("itsadug")) install.packages("itsadug")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("gtsummary")) install.packages("gtsummary")
if (!require("meta")) install.packages("meta")
if (!require("metaviz")) install.packages("metaviz")
if (!require("brms")) install.packages("brms")
if (!require("patchwork")) install.packages("patchwork")

## Fig.S1. Effect of inflation, debt, and assets on temporal discount scores.

score_ef1 <- gam_graph(results_scores_brm$mg1, 1, -1, 2, "Inflation", "Temporal discout scores")
score_ef2 <-gam_graph(results_scores_brm$mg1, 2, -1,  2, "Debt", "Temporal discout scores")
score_ef3 <-gam_graph(results_scores_brm$mg1, 3, -1, 2,"Assets", "Temporal discout scores")

FigS1 <- (score_ef1/score_ef2 + score_ef3) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 12,
                                face = "bold"))

ggsave(filename = "FigS1.tiff", width = 6, height = 10, device = "tiff",dpi=400)

########################
# 2. Figure S2
########################

## Fig S2. Country-level effects of country-inequality on scores, scores per type of item, and anomalies per country.

anom_ef11 <-gam_graph(results_anomalies_brm[[1]]$mg1, 1, -2.5, 2,"Inflation", "Present bias")
anom_ef12 <-gam_graph(results_anomalies_brm[[1]]$mg1, 2, -4, 2, "Assets", "Present bias")

anom_ef21 <-gam_graph(results_anomalies_brm[[2]]$mg1, 1,  -2.5, 2,"Inflation", "Absolute magnitude")
anom_ef22 <-gam_graph(results_anomalies_brm[[2]]$mg1, 2,  -1.5, 4,"Assets", "Absolute magnitude")

anom_ef31 <-gam_graph(results_anomalies_brm[[3]]$mg1, 1, -2.5, 2,"Inflation", "Gain-loss assymetry")
anom_ef32 <-gam_graph(results_anomalies_brm[[3]]$mg1, 2,  -2, 4,"Assets", "Gain-loss assymetry")

anom_ef41 <-gam_graph(results_anomalies_brm[[4]]$mg1, 1, -2.5, 2,"Inflation", "Delay-speedup")
anom_ef42 <-gam_graph(results_anomalies_brm[[4]]$mg1, 2, -4, 2,"Assets", "Delay-speedup")

anom_ef51 <-gam_graph(results_anomalies_brm[[5]]$mg1, 1, -2.5, 2,"Inflation", "Subadditivity")
anom_ef52 <-gam_graph(results_anomalies_brm[[5]]$mg1, 2, -4, 2,"Assets", "Subadditivity")

FigS2 <- ((anom_ef11 + anom_ef12) / 
            (anom_ef21 + anom_ef22) / 
            (anom_ef31 + anom_ef32) /
            (anom_ef41 + anom_ef42) / 
            (anom_ef51 + anom_ef52)) +
  plot_annotation(tag_levels = 'A') & 
  plot_layout(guides = "collect")  +
  theme(legend.position = 'bottom',
        plot.tag = element_text(size = 12, face = 'bold'))

ggsave(filename = "FigS2.tiff", width = 8, height = 14, device = "tiff", dpi=400)

########################
# 3. Figure S3-S8
########################

## Figure S3 - S8: Plots of random-effects effects meta-analyses. Plots will be automatically saved on the working directoriy in tiff format.

dat_test <- dat_filter %>% 
  group_by(Code) %>% 
  summarise(mean = mean(score),
            sd = sd(score),
            n = n())

### Random-effects meta-analysis for average temporal discount
meta_scores <- metamean(mean = mean,
                        sd = sd,
                        n = n, 
                        studlab=Code, 
                        data = dat_test,
                        method.tau="REML",
                        prediction = T)

dev.off()
tiff("FigS3.tiff", width = 20, height = 38, units = "cm", res = 300)
forest.meta(meta_scores, 
            sortvar =  mean,
            xlab="Temporal discount scores", 
            comb.r=T, 
            comb.f=F, 
            predict = T,
            fontsize=12, 
            digits=2,
            xlim = c(0,19),
            weight.study = "random",
            col.square.lines =  "black", 
            col.study = "black",
            col.inside = "black")


### Random-effects meta-analysis for each anonamaly
meta_anoms <- list()
vars <- c("presbias", "absolmag", "gainloss", "delayspeed", "subaddit")
for (i in 1:length(vars)) {
  meta_anoms[[i]] <- metanal_anom_prop(dat_filter, vars[i])
  tiff(paste0("FigS",i+3,".tiff", collapse = "") , width = 24, height = 38, units = "cm", res = 300)
  meta_graph_anom <- forest.meta(meta_anoms[[i]], 
                                 sortvar = TE,
                                 xlab="Proportion", 
                                 random=T, 
                                 comb.f=F, 
                                 predict = T,
                                 xlim = c(0,1), 
                                 fontsize=12, 
                                 digits=2,
                                 col.square.lines =  "black", 
                                 col.study = "black",
                                 col.inside = "black")
  dev.off()
}

########################
# 4. Figure S9
########################

## Figure F9. Individual and country-level correlations for temporal discount scores and anomalies. 

Table_S9_individual_level <- dat_filter %>% 
  select(score, presbias, absolmag, gainloss, delayspeed, subaddit) %>% 
  rename("TDS" = "score", 
         "PB" = "presbias",
         "AM" = "absolmag",
         "GLS" = "gainloss",
         "DS" = "delayspeed",
         "SA" = "subaddit") %>% 
  na.omit(.) %>% 
  correlation::correlation(., 
                           method = "pearson",
                           p_adjust = "holm")


Table_S9_country_level <- dat_filter %>% 
  group_by(Residence) %>% 
  summarise(score = mean(score, na.rm =T), 
            presbias = mean(presbias, na.rm =T),
            absolmag = mean(absolmag, na.rm =T),
            gainloss = mean(gainloss, na.rm =T),
            delayspeed = mean(delayspeed, na.rm =T),
            subaddit = mean(subaddit, na.rm =T)) %>% 
  
  ungroup %>% 
  select(score, presbias, absolmag, gainloss, delayspeed, subaddit) %>% 
  rename("TDS" = "score", 
         "PB" = "presbias",
         "AM" = "absolmag",
         "GLS" = "gainloss",
         "DS" = "delayspeed",
         "SA" = "subaddit") %>% 
  drop_na(.) %>% 
  correlation::correlation(., 
                           method = "pearson",
                           p_adjust = "holm")

FigS9A <- summary(Table_S9_individual_level, redundant = TRUE) %>% 
  plot(type = "tile", 
       show_values = TRUE, 
       show_p = TRUE)

FigS9B <- summary(Table_S9_country_level, redundant = TRUE) %>% 
  plot(type = "tile", 
       show_values = TRUE, 
       show_p = TRUE)

FigS9 <- FigS9A / FigS9B + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        plot.tag = element_text(size = 9, 
                                face = 'bold')) 

ggsave(FigS9A, filename = "cntry.levelcorr.tiff", width = 8, height = 12, device = "tiff", dpi=400)

########################
# 5. Figure S10
########################

## Fig s10. Robustness to number of knots on smooth-term estimation. 
## We employ the Bayesian versions for this plot as the posterior draws allow for a better inspection of potential departures due to non-modeled non-linearities.

FigS10A <- conditional_smooths(results_scores_brm$mg1, 
                               method = "fitted",
                               re_formula="NULL",
                               rug = T,
                               mean = T,
                               spaghetti = T,
                               ndraws = 1000,
                               points = T) 

FigS10B <- conditional_smooths(results_scores_brm_k9$mg1, 
                               method = "fitted",
                               re_formula="NULL",
                               rug = T,
                               mean = T,
                               ndraws = 1000,
                               spaghetti = T,
                               points = T) 

g11 <- plot(FigS10A, plot = FALSE)[[1]]+
  bbc_theme() +
  ylim(-1,2)+ 
  xlab("Inflation") + 
  ylab("Effect on temporal discount scores")+
  theme(axis.text = element_text(face = "bold"))

g12 <- plot(FigS10A, plot = FALSE)[[2]]+
  ylim(-1,2)+ 
  bbc_theme() +
  xlab("Debt") + 
  ylab("Effect on temporal discount scores")+
  theme(axis.text = element_text(face = "bold"))

g13 <- plot(FigS10A, plot = FALSE)[[3]]+
  ylim(-1,2)+ 
  bbc_theme() +
  xlab("Assets") + 
  ylab("Effect on temporal discount scores")

g21 <- plot(FigS10B, plot = FALSE)[[1]]+
  ylim(-1,2)+ 
  bbc_theme() +
  xlab("Inflation") + 
  ylab("Effect on temporal discount scores")

g22 <- plot(FigS10B, plot = FALSE)[[2]]+
  ylim(-1,2)+ 
  bbc_theme() +
  xlab("Debt") + 
  ylab("Effect on temporal discount scores")

g23 <- plot(FigS10B, plot = FALSE)[[3]]+
  ylim(-1,2)+ 
  bbc_theme() +
  xlab("Assets") + 
  ylab("Effect on temporal discount scores")

FigS10 <- (g11+g12+g13)/(g21+g22+g23)  +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10,
                                face = "bold"))

FigS10 <- ggsave(FigS10,
           filename = "smooth_comp.tiff",
           width = 14,
           height = 8,
           device='tiff', 
           dpi=400)

########################
# 5. Table  S3
########################

## This code will reproduce Table S3 in HTML format. This table includes the sample descriptive information.

Table_S3 <- dat_unique %>% 
  select(Residence, Age, Gender, EducationCompleted, Employment) %>% 
  
  mutate(Gender = fct_recode(Gender, "Other" = "Prefer not to answer"),
         EducationCompleted = fct_recode(EducationCompleted, 
                                         "Primary ed." = "No formal ed.",
                                         "Graduate" = "MBA",  
                                         "Graduate" = "PhD"),
         Employment = fct_recode(Employment, 
                                 "Employed full-time" = "Military")) %>% 
  
  mutate(
    Gender = fct_relevel(Gender, "Female", "Male", "Other"),
    EducationCompleted = fct_relevel(EducationCompleted, 
                                     "Primary ed.", 
                                     "Secondary ed.", 
                                     "Technical ed.",
                                     "Bachelor",  
                                     "Graduate"),
    
    Employment = fct_relevel(Employment, 
                             "Full-time student",
                             "Not in paid employment (looking)",
                             "Not in paid employment (personal reasons)",
                             "Employed part-time",
                             "Employed full-time",
                             "Self-employed",
                             "Retired")) %>%  
  tbl_summary(
    by = Residence,
    missing = "no",
    list(all_continuous() ~ "{mean} ({sd})",
         all_categorical() ~ "{p}%"),
    digits = list(all_continuous() ~ 1, all_categorical() ~ 1),
    label = EducationCompleted ~ "Highest education level",
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_overall() %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 

########################
# 6. Table S4
########################

## This code will reproduce Table S4 in HTML format. 
## This table includes the descriptive information for the temporal discount scores and anomalies rates. 
## In this code, we divide the tables for scores and anomalies to improve its visualization. 

gains <- get_scores(get_latest_answer(dat_item, 6))$score
payments <- 5 -get_scores(get_latest_answer(dat_item, 11))$score
largergains <- get_scores(get_latest_answer(dat_item, 16))$score

q <- dat_item %>% 
  count(ResponseId) %>% 
  arrange()        

Table_S4_scores <- dat_item %>% 
  arrange(ResponseId) %>%
  mutate (gains = rep(gains, q$n),
          payments = rep(payments, q$n),
          largergains = rep(largergains, q$n)) %>% 
  select(Residence, gains, payments, largergains) %>% 
  tbl_summary(
    by = Residence,
    type = all_categorical() ~ "continuous2",
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no",
    digits = all_continuous() ~ 1,
  ) %>% 
  # add column with total number of non-missing observations
  add_overall() %>% # add column with total number of non-missing observations
  #  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 

rm(gains, payments, largergains, q)

Table_S4_anomalies <- dat_unique %>% 
  select(Residence, presbias, absolmag, delayspeed, gainloss, subaddit) %>% 
  tbl_summary(
    by = Residence,
    type = all_categorical() ~ "continuous2",
    statistic = all_continuous() ~ "{mean}",
    label = list(
      presbias ~ "Present bias", 
      absolmag ~ "Absolute magnitude", 
      gainloss ~ "Gain-loss asymmetry", 
      delayspeed ~ "Delay-speedup", 
      subaddit ~ "Subadditivity"),
    missing = "no",
    digits = all_continuous() ~ 2,
  ) %>% 
  # add column with total number of non-missing observations
  add_overall() %>% # add column with total number of non-missing observations
  #  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()

########################
# 7. Table  S5
########################

# This section will reproduce Table S5 in HTML format. 
## This table includes the descriptive information for model fit comparison between a null, random-intercept and random slopes models in each case.

job::job({
  results_scores_bam_comparison <- dat_filter %>% 
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
           assets.cwc ,
    ) %>% 
    bam_estimation_comparison(., bs = "cs", family = "gaussian")
}, import = c (bam_estimation_comparison, dat_filter))

### Comparison for temporal discount scores
compareML(results_scores_bam_comparison[[1]], 
          results_scores_bam_comparison[[2]])

compareML(results_scores_bam_comparison[[2]], 
          results_scores_bam_comparison[[3]])

job::job ({
  results_anomalies_bam_comparison <- map(dat_anoms, bam_estimation_comparison, bs = "cs", family = "binomial")
}, import = c (bam_estimation_comparison, dat_anoms))

### Comparison for present bias
compareML(results_anomalies_bam_comparison[[1]][[1]], 
          results_anomalies_bam_comparison[[1]][[2]])

compareML(results_scores_bam_comparison[[1]][[2]], 
          results_scores_bam_comparison[[1]][[3]])

### Comparison for absolute magnitude
compareML(results_anomalies_bam_comparison[[2]][[1]], 
          results_anomalies_bam_comparison[[2]][[2]])

compareML(results_scores_bam_comparison[[2]][[2]], 
          results_scores_bam_comparison[[2]][[3]])

### Comparison for gain-loss asymmetry
compareML(results_anomalies_bam_comparison[[3]][[1]], 
          results_anomalies_bam_comparison[[3]][[2]])

compareML(results_scores_bam_comparison[[3]][[2]], 
          results_scores_bam_comparison[[3]][[3]])

### Comparison forDelay-speedup
compareML(results_anomalies_bam_comparison[[4]][[1]], 
          results_anomalies_bam_comparison[[4]][[2]])

compareML(results_scores_bam_comparison[[4]][[2]], 
          results_scores_bam_comparison[[4]][[3]])

### Comparison for Subadditivity
compareML(results_anomalies_bam_comparison[[5]][[1]], 
          results_anomalies_bam_comparison[[5]][[2]])

compareML(results_scores_bam_comparison[[5]][[2]], 
          results_scores_bam_comparison[[5]][[3]])

########################
# 8. Table  S6
########################

## Results for Tables S6, S8-S12 can be assessed using the estimated models in the model estimation script.

########################
# 9. Table  S7
########################

## This code will reproduce Table S7, which estimates a gam model similar to our model scheme comparison but using a regularized thin-plate penalization (instead of a regularized cubic spline). 
## Models results can be inspected using the corresponding object.

job::job({
  results_scores_bam_ts <- dat_filter %>% 
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
           assets.cwc ,
           
    ) %>% 
    bam_estimation_comparison(., bs = "ts", family = "gaussian")
}, import = c (bam_estimation_comparison, dat_filter))


job::job ({
  results_anomalies_bam_ts <- map(dat_anoms, bam_estimation, bs = "ts", family = "binomial")
}, import = c (bam_estimation, dat_anoms))


########################
# 10. Table  S13
########################

## This code will reproduce Table S13, which estimates the effects of all predictors (similar to our main models) on the number of anomalies presented by each person. 
## For these anomalies, subadditivity is removed. Results for Table_S13 can be assessed in the estimated GAM object. 

Table_S13 <- dat_filter %>% 
  rowwise() %>%
  mutate(sum_ano = sum(presbias, absolmag, delayspeed, gainloss)) 

Table_S13$sum.gmcs = c(scale(Table_S13$sum_ano))

job::job({
  results_sum_anom_lmer <- Table_S13 %>% 
    select(sum.gmcs, 
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
}, import = c(Table_S13, fit_models_lmer_ns))

########################
# 11. Table  S14
########################

## This code will reproduce Table S14, which estimates the potential non-linear of GDP on each block of items of the temporal discount scores. 
## We employed GAM models to estimate these effects. Table results can be assesed using the estimated models. 

gains <- get_scores(get_latest_answer(dat_item, 6))$score
payments <- 5-get_scores(get_latest_answer(dat_item, 11))$score
largergains <- get_scores(get_latest_answer(dat_item, 16))$score

q <- dat_item %>% 
  count(ResponseId) %>% 
  arrange()        

gdpbam <- dat_item %>% 
  arrange(ResponseId) %>%
  mutate (gains = rep(gains, q$n),
          payments = rep(payments, q$n),
          largergains = rep(largergains, q$n)) %>% 
  select(Code, GDP, Cntry_income, gains, payments, largergains) %>% 
  mutate(GDP.log = log(GDP)) %>% 
  group_by(Code) %>% 
  summarise(GDP.log = mean(GDP.log),
            gains = mean(gains),
            payments = mean(payments),
            largergains = mean(largergains)) %>% 
  ungroup()


m_gains_gdp <- bam(gains ~ s(GDP.log, bs = "cs"),    
                   data = gdpbam,
                   family = "gaussian")
m_payment_gdp <- bam(payments ~ s(GDP.log, bs = "cs"),    
                     data = gdpbam,
                     family = "gaussian")
m_largergains_gdp <- bam(largergains ~ s(GDP.log, bs = "cs"),    
                         data = gdpbam,
                         family = "gaussian")

########################
# 12. Table  S15
########################

## This code will reproduce Table S15, including information on the number of anomalies and percentage of the sample overall and by-country engaging in each category. 
## We separate global from by-country results to improve visualization.

Table_S15 <- dat_unique %>% 
  rowwise() %>%
  mutate(sum_ano = sum(presbias, absolmag, delayspeed, gainloss)) %>%
  select(Residence, sum_ano) %>%
  mutate(sum_ano = as.factor(sum_ano)) %>%
  mutate(sum_ano = fct_relevel(sum_ano, "0", "1", "2", "3", "4")) %>% 
  group_by(Residence) %>% 
  count(sum_ano) %>% 
  mutate(value = n /sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = "sum_ano", values_from = value) %>% 
  select(-"NA") %>% 
  mutate(across(where(is.numeric), round, 2))

overall <- Table_S15 %>% 
  ungroup %>%  
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

htmlTable::htmlTable(Table_S15)

########################
# 13. Table  S16
########################

## This code will estimate the models presented in Table S16. For each predictor, we estimate a gam model using bam() to predict aggregate scores at country-levels. 
## Results from the table can be obtained by using the summary() function.

dat_analyis_country <- dat_filter %>% 
  mutate(debt3 = debt/Country_income) %>% 
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  filter(debt3 < 100)

gdpbam <- dat_analyis_country %>% 
  group_by(Code) %>% 
  summarise(score = mean(score.gmcs),
            GINI = mean(GINI),
            GDP = mean(GDP.log),
            Inflation = mean(Inflation),
            indineq = mean(indineq),
            assets = mean(assets),
            debt = mean(debt3),) %>% 
  ungroup()


m_GINI <- bam(score ~ s(GINI, bs = "cs"),    
              data = gdpbam,
              family = "gaussian")
m_GDP <- bam(score ~ s(GDP, bs = "cs"),    
             data = gdpbam,
             family = "gaussian")
m_inflation <- bam(score ~ s(Inflation, bs = "cs"),    
                   data = gdpbam,
                   family = "gaussian")
m_indineq <- bam(score ~ s(indineq, bs = "cs"),    
                 data = gdpbam,
                 family = "gaussian")
m_assets <- bam(score ~ s(assets, bs = "cs"),    
                data = gdpbam,
                family = "gaussian")
m_debt <- bam(score ~ s(debt, bs = "cs"),    
               data = gdpbam,
               family = "gaussian")




########################
# 14. Table  S17
########################

## This code will estimate the models presented in Table S17. We reproduced our main analyses using more stringent criteria for response times, income and assets. 
## Results from the table can be obtained by using the summary() function.

dat_filter_cons <- dat_unique %>% 
  
  group_by(Code) %>% 
  
  #Remove additional ~100 improbable responses      
  filter(income - median(income, na.rm =T) < 25*mad(income, na.rm = T)) %>% 
  filter(assets - median(assets, na.rm =T) < 250*mad(assets, na.rm = T)) %>% 
  
  #Remove additional ~250 improbable responses 
  filter(!(income < 1 & (Employment == "Employed full-time" | Employment == "Employed part-time"))) %>%
  
  ## We mean center and standardize all our individual-level variables
  mutate(age.cwc = Age-mean(Age, na.rm = T)) %>%
  mutate(age.cwc = c(scale(age.cwc))) %>%
  
  mutate(indineq.cwc = indineq-mean(indineq)) %>%
  mutate(indineq.cwc = c(scale(indineq.cwc))) %>%
  
  mutate(Risk_preference.cwc = Risk_preference-mean(Risk_preference)) %>%
  mutate(Risk_preference.cwc = c(scale(Risk_preference.cwc))) %>%
  
  mutate(income.cwc = income-mean(income)) %>%
  mutate(income.cwc = c(scale(income.cwc))) %>%
  
  mutate(debt.cwc = debt-mean(debt)) %>%
  mutate(debt.cwc = c(scale(debt.cwc))) %>%
  
  mutate(assets.cwc = assets-mean(assets)) %>%
  mutate(assets.cwc = c(scale(assets.cwc))) %>% 
  
  mutate(disc_debt.cwc = disc_debt-mean(disc_debt)) %>%
  mutate(disc_debt.cwc = c(scale(disc_debt.cwc))) %>%
  
  mutate(disc_invest.cwc = disc_invest-mean(disc_invest)) %>%
  mutate(disc_invest.cwc = c(scale(disc_invest.cwc))) %>%
  
  mutate(disc_savings.cwc = disc_savings-mean(disc_savings)) %>%
  mutate(disc_savings.cwc = c(scale(disc_savings.cwc))) %>%
  
  mutate(disc_non_fin.cwc = disc_non_fin-mean(disc_non_fin)) %>%
  mutate(disc_non_fin.cwc = c(scale(disc_non_fin.cwc))) %>% 
  
  
  ungroup() %>% 
  
  ## We grand-mean center and standardize all our individual-level variables
  mutate(score.gmcs = c(scale(score))) %>%
  mutate(GINI.gmc = GINI-mean(GINI, na.rm = T)) %>%
  mutate(GINI.gmc = c(scale(GINI.gmc))) %>% 
  
  mutate(GDP.log = log(GDP)) %>%
  mutate(GDP.gmc = GDP.log-mean(GDP.log, na.rm = T)) %>%
  mutate(GDP.gmc = c(scale(GDP.gmc))) %>%
  
  mutate(inflation.gmc = Inflation-mean(Inflation, na.rm = T)) %>%
  mutate(inflation.gmc = c(scale(inflation.gmc))) %>%
  
  ## We include a last changes of some relevant variables, including changing the order of certain levels and changes in labeling.
  mutate(Gender = fct_recode(Gender, "Other" = "Prefer not to answer"),
         EducationCompleted = fct_recode(EducationCompleted, 
                                         "Primary ed." = "No formal ed.",
                                         "Graduate" = "MBA", 
                                         "Graduate" = "PhD"),
         Employment = fct_recode(Employment, 
                                 "Employed full-time" = "Military")) %>% 
  
  mutate(
    Gender = fct_relevel(Gender, "Female", "Male", "Other"),
    EducationCompleted = fct_relevel(EducationCompleted, 
                                     "Bachelor", 
                                     "Primary ed.", 
                                     "Secondary ed.", 
                                     "Technical ed.",
                                     "Graduate"),
    
    Employment = fct_relevel(Employment, 
                             "Employed full-time",
                             "Self-employed",
                             "Employed part-time",
                             "Not in paid employment (looking)",
                             "Not in paid employment (personal reasons)",
                             "Full-time student",
                             "Retired"),
    
    Majority = fct_relevel(Majority, 
                           "Majority",
                           "Minority",
                           "Immigrant",
                           "Mixed race"))

if (!require("lme4")) install.packages("lme4")
if (!require("lmerTest")) install.packages("lmerTest")
if (!require("emmeans")) install.packages("emmeans")
if (!require("gamm4")) install.packages("gamm4") 
if (!require("brms")) install.packages("brms") 
if (!require("cmdstanr")) install.packages("cmdstanr") 
if (!require("sjPlot")) install.packages("sjPlot")



if (!require("mgcv")) install.packages("mgcv")
if (!require("gratia")) install.packages("gratia")
if (!require("itsadug")) install.packages("itsadug")
if (!require("htmlTable")) install.packages("htmlTable")
if (!require("job")) install.packages("job")


## Estimation of effects for temporal discount scores 
### Details of the estimation are provided in Supplementary materials.

job::job({
  ### We employ the job package to free the console during the estimation of these models.
  results_scores_bam_cons <- dat_filter_cons %>% 
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
}, import = c (bam_estimation, dat_filter_cons))

## Estimation of effects for each anomaly.
### We employ the job package to free the console during the estimation of these models.
### Details of the estimation are provided in Supplementary materials.

dat_anoms_cons <- list()
vars <- c("presbias", "absolmag", "gainloss", "delayspeed", "subaddit")
for (i in 1:length(vars)) {
  dat_anoms_cons[[i]] <- dat_filter_cons %>% 
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
  results_anomalies_bam_cons <- map(dat_anoms_cons, 
                                 bam_estimation, bs = "cs", family = "binomial")
}, import = c (bam_estimation, dat_anoms_cons))


## For interested readers, we plot the summary of each  model. HTML verions of the same are displayed below.
score_gam <- htmlTable(gamtabs(results_scores_bam_cons[[1]], caption='Summary of scores gamm', type = "HTML")); score_gam
pbias_gam <- htmlTable(gamtabs(results_anomalies_bam_cons[[1]][[1]], caption='Summary of present bias gamm', type = "HTML")); pbias_gam
absol_gam <- htmlTable(gamtabs(results_anomalies_bam_cons[[2]][[1]], caption='Summary of absolute magnitude gamm', type = "HTML")); absol_gam
gainloss_gam <- htmlTable(gamtabs(results_anomalies_bam_cons[[3]][[1]], caption='Summary of gain-loss assymetry gamm', type = "HTML")); gainloss_gam
delay_gam <- htmlTable(gamtabs(results_anomalies_bam_cons[[4]][[1]], caption='Summary of delay-speedup gamm', type = "HTML"));delay_gam
subad_gam <-htmlTable(gamtabs(results_anomalies_bam_cons[[5]][[1]], caption='Summary of subadditivity gamm', type = "HTML")); subad_gam



dat_filter_cons2 <- dat_unique %>% 
  
  group_by(Code) %>% 
  
  #Remove additional ~100 improbable responses      
  filter(income - median(income, na.rm =T) < 10*mad(income, na.rm = T)) %>% 
  filter(assets - median(assets, na.rm =T) < 100*mad(assets, na.rm = T)) %>% 
  
  #Remove additional ~250 improbable responses 
  filter(!(income < 1 & (Employment == "Employed full-time" | Employment == "Employed part-time"))) %>%
  
  ## We mean center and standardize all our individual-level variables
  mutate(age.cwc = Age-mean(Age, na.rm = T)) %>%
  mutate(age.cwc = c(scale(age.cwc))) %>%
  
  mutate(indineq.cwc = indineq-mean(indineq)) %>%
  mutate(indineq.cwc = c(scale(indineq.cwc))) %>%
  
  mutate(Risk_preference.cwc = Risk_preference-mean(Risk_preference)) %>%
  mutate(Risk_preference.cwc = c(scale(Risk_preference.cwc))) %>%
  
  mutate(income.cwc = income-mean(income)) %>%
  mutate(income.cwc = c(scale(income.cwc))) %>%
  
  mutate(debt.cwc = debt-mean(debt)) %>%
  mutate(debt.cwc = c(scale(debt.cwc))) %>%
  
  mutate(assets.cwc = assets-mean(assets)) %>%
  mutate(assets.cwc = c(scale(assets.cwc))) %>% 
  
  mutate(disc_debt.cwc = disc_debt-mean(disc_debt)) %>%
  mutate(disc_debt.cwc = c(scale(disc_debt.cwc))) %>%
  
  mutate(disc_invest.cwc = disc_invest-mean(disc_invest)) %>%
  mutate(disc_invest.cwc = c(scale(disc_invest.cwc))) %>%
  
  mutate(disc_savings.cwc = disc_savings-mean(disc_savings)) %>%
  mutate(disc_savings.cwc = c(scale(disc_savings.cwc))) %>%
  
  mutate(disc_non_fin.cwc = disc_non_fin-mean(disc_non_fin)) %>%
  mutate(disc_non_fin.cwc = c(scale(disc_non_fin.cwc))) %>% 
  
  
  ungroup() %>% 
  
  ## We grand-mean center and standardize all our individual-level variables
  mutate(score.gmcs = c(scale(score))) %>%
  mutate(GINI.gmc = GINI-mean(GINI, na.rm = T)) %>%
  mutate(GINI.gmc = c(scale(GINI.gmc))) %>% 
  
  mutate(GDP.log = log(GDP)) %>%
  mutate(GDP.gmc = GDP.log-mean(GDP.log, na.rm = T)) %>%
  mutate(GDP.gmc = c(scale(GDP.gmc))) %>%
  
  mutate(inflation.gmc = Inflation-mean(Inflation, na.rm = T)) %>%
  mutate(inflation.gmc = c(scale(inflation.gmc))) %>%
  
  ## We include a last changes of some relevant variables, including changing the order of certain levels and changes in labeling.
  mutate(Gender = fct_recode(Gender, "Other" = "Prefer not to answer"),
         EducationCompleted = fct_recode(EducationCompleted, 
                                         "Primary ed." = "No formal ed.",
                                         "Graduate" = "MBA", 
                                         "Graduate" = "PhD"),
         Employment = fct_recode(Employment, 
                                 "Employed full-time" = "Military")) %>% 
  
  mutate(
    Gender = fct_relevel(Gender, "Female", "Male", "Other"),
    EducationCompleted = fct_relevel(EducationCompleted, 
                                     "Bachelor", 
                                     "Primary ed.", 
                                     "Secondary ed.", 
                                     "Technical ed.",
                                     "Graduate"),
    
    Employment = fct_relevel(Employment, 
                             "Employed full-time",
                             "Self-employed",
                             "Employed part-time",
                             "Not in paid employment (looking)",
                             "Not in paid employment (personal reasons)",
                             "Full-time student",
                             "Retired"),
    
    Majority = fct_relevel(Majority, 
                           "Majority",
                           "Minority",
                           "Immigrant",
                           "Mixed race"))

## Estimation of effects for temporal discount scores 
### Details of the estimation are provided in Supplementary materials.

job::job({
  ### We employ the job package to free the console during the estimation of these models.
  results_scores_bam_cons2 <- dat_filter_cons2 %>% 
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
}, import = c (bam_estimation, dat_filter_cons2))

## Estimation of effects for each anomaly.
### We employ the job package to free the console during the estimation of these models.
### Details of the estimation are provided in Supplementary materials.

dat_anoms_cons2 <- list()
vars <- c("presbias", "absolmag", "gainloss", "delayspeed", "subaddit")
for (i in 1:length(vars)) {
  dat_anoms_cons2[[i]] <- dat_filter_cons2 %>% 
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
  results_anomalies_bam_cons <- map(dat_anoms_cons2, 
                                    bam_estimation, bs = "cs", family = "binomial")
}, import = c (bam_estimation, dat_anoms_cons2))


## For interested readeres, we plot the summary of each  model. HTML verions of the same are displayed below.
score_gam <- htmlTable(gamtabs(results_scores_bam_cons2[[1]], caption='Summary of scores gamm', type = "HTML")); score_gam
pbias_gam <- htmlTable(gamtabs(results_anomalies_bam_cons2[[1]][[1]], caption='Summary of present bias gamm', type = "HTML")); pbias_gam
absol_gam <- htmlTable(gamtabs(results_anomalies_bam_cons2[[2]][[1]], caption='Summary of absolute magnitude gamm', type = "HTML")); absol_gam
gainloss_gam <- htmlTable(gamtabs(results_anomalies_bam_cons2[[3]][[1]], caption='Summary of gain-loss assymetry gamm', type = "HTML")); gainloss_gam
delay_gam <- htmlTable(gamtabs(results_anomalies_bam_cons2[[4]][[1]], caption='Summary of delay-speedup gamm', type = "HTML"));delay_gam
subad_gam <-htmlTable(gamtabs(results_anomalies_bam_cons2[[5]][[1]], caption='Summary of subadditivity gamm', type = "HTML")); subad_gam

