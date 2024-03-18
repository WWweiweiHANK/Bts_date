##R code for analyses in Study 2c of "The Illusion of Moral Decline" (Mastroianni & Gilbert, 2023)

#####IMPORTANT NOTES#####
##DO NOT open this code file by dragging it into a pre-existing R session.
##Instead, open the .rproj file
##This project is meant to be self-contained, and packages will install and load as necessary.
##However, it will not work unless opened via the .rproj file.
##Session info is available at the end of this document.
##if you encounter any issues, please email Adam Mastroianni at adam.m.mastroianni@gmail.com
#####SETUP#####

##Set seed to make any analyses involving random sampling reproducible
addTaskCallback(function(...) {set.seed(123);TRUE})
##Note that this will make the bootstraps return the same numbers each time you run them
##if you set the seed normally with set.seed(), running the line again will naturally return a different result

options(scipen = 999) ##prevent scientific notation

require(ggplot2)
require(dplyr)
require(plyr)
require(lme4)
require(lmerTest)
require(tidyr)
require(emmeans)

dat <- read.csv("study2c.csv")
head(dat)
dim(dat)

#####data cleaning, remove incompletes#####
dat$inclusion <- NULL
dat$reason <- NULL

dat$inclusion[dat$Finished == "FALSE"] <- "exclude"
dat$reason[dat$Finished == "FALSE"] <- "incomplete"
table(dat$inclusion)

dat <- dat[is.na(dat$inclusion),]

##quota'd out
dat$inclusion[dat$footwear == ""] <- "exclude"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]

##english test
dat$inclusion[dat$footwear != "Bell bottoms"] <- "exclude"
dat$inclusion[dat$rsvp != "A wedding invitation"] <- "exclude"
dat$inclusion[dat$elevator != "An elevator"] <- "exclude"
dat$reason[dat$inclusion == "exclude"] <- "english"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]
dim(dat)

#####report initial demographics####
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

####exclusions####
##attention check
dat$inclusion[dat$check != "Other"] <- "exclude"
dat$reason[dat$check != "Other"] <- "check"
dat$inclusion[dat$check_4_TEXT == ""] <- "exclude"
dat$check_4_TEXT
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]

##age check
levels(dat$age_1)
table(dat$inclusion)
dat$inclusion[dat$age_1 == "18-24" & (dat$age < 18 | dat$age > 24)] <- "exclude"
dat$inclusion[dat$age_1 == "25-29" & (dat$age < 25 | dat$age > 29)] <- "exclude"
dat$inclusion[dat$age_1 == "30-34" & (dat$age < 30 | dat$age > 34)] <- "exclude"
dat$inclusion[dat$age_1 == "35-39" & (dat$age < 35 | dat$age > 39)] <- "exclude"
dat$inclusion[dat$age_1 == "40-44" & (dat$age < 40 | dat$age > 44)] <- "exclude"
dat$inclusion[dat$age_1 == "45-49" & (dat$age < 45 | dat$age > 49)] <- "exclude"
dat$inclusion[dat$age_1 == "50-54" & (dat$age < 50 | dat$age > 54)] <- "exclude"
dat$inclusion[dat$age_1 == "55-59" & (dat$age < 55 | dat$age > 59)] <- "exclude"
dat$inclusion[dat$age_1 == "60-64" & (dat$age < 60 | dat$age > 64)] <- "exclude"
dat$inclusion[dat$age_1 == "65-69" & (dat$age < 65 | dat$age > 69)] <- "exclude"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]

##manipulation check
summary(dat$today)
dat$born <- coalesce(dat$X2000_born, dat$X1995_born, dat$X1990_born,
                     dat$X1985_born, dat$X1980_born, dat$X1975_born, 
                     dat$X1970_born, dat$X1965_born,
                     dat$X1960_born, dat$X1955_.born)
summary(dat$born)
dat$twenty <- coalesce(dat$X2015, dat$X2010, dat$X2005, dat$X2000, dat$X1995, dat$X1990,
                       dat$X1985, dat$X1980, dat$X1975, 
                       dat$X1970, dat$X1965,
                       dat$X1960, dat$X1955)
summary(dat$twenty)

dat$diff <- dat$today - dat$born
dat$diff_born <- dat$today - dat$born
dat$diff_20 <- dat$today - dat$twenty
##diff is difference between today and when you were born

dat$diff_cat <- NULL
dat$diff_cat[dat$diff == 0] <- "equal"
dat$diff_cat[dat$diff > 0] <- "more"
dat$diff_cat[dat$diff < 0] <- "less"

dat$cat <- paste(dat$check_20, dat$check_25, dat$check_30,
                    dat$check_35, dat$check_40, dat$check_45,
                    dat$check_50, dat$check_55, dat$check_60,
                    dat$check_65, sep = "")
dat$cat <- as.factor(dat$cat)
levels(dat$cat)

dat$cat2 <- mapvalues(dat$cat, from = c("People are equally kind, honest, nice, and good today compared to about 20 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 25 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 30 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 35 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 40 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 45 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 50 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 55 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 60 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 65 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 20 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 25 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 30 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 35 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 40 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 45 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 50 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 55 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 60 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 65 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 20 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 25 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 30 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 35 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 40 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 45 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 50 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 55 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 60 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 65 years ago"),
                      to = c("equal","equal","equal","equal","equal","equal","equal","equal","equal","equal",
                             "less","less","less","less","less","less","less","less","less","less",
                             "more","more","more","more","more","more","more","more","more","more"))
table(dat$cat2, dat$diff_cat)
dat$inclusion[dat$cat2 != dat$diff_cat] <- "exclude"
table(dat$inclusion)
dim(dat)

##creating differences
dat$born_minus_twenty <- dat$born - dat$twenty
dat$twenty_minus_today <- dat$twenty - dat$today
dat$born_minus_today <- dat$born - dat$today

#####re-report demographics after exclusions#####
dat <- dat[is.na(dat$inclusion),]

dim(dat)
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

##reshape
good_melt <- reshape(dat, varying = c("today","twenty","born"),
                     v.names = "rating", timevar = "time",
                     times = c("today","twenty","born"),
                     direction = "long",
                     idvar = "participant")
good_melt$time <- as.factor(good_melt$time)

#####model#####
good_melt$time <- factor(good_melt$time, levels(good_melt$time)[c(3,2,1)])
good_mod <- lmer(rating ~ time + (1|participant), data = good_melt)
summary(good_mod)

means <- emmeans(good_mod, specs = ~ time)
means
eff_size(means, sigma = sigma(good_mod), edf = 677)
contr <- contrast(means, method = "pairwise", adjust = "holm")
contr
confint(contr)

#####decline per year#####
dat$dpy_20_today <- (dat$today - dat$twenty)/(dat$age - 20)
dat$dpy_20_born <- (dat$twenty - dat$born)
dat$dpy_total <- (dat$today - dat$born)/(dat$age)

dpy_melt <- reshape(dat, varying = c("dpy_20_today","dpy_20_born", "dpy_total"),
                    v.names = "dpy", timevar = "type", times = c("dpy_20_today","dpy_20_born","dpy_total"),
                    direction = "long", idvar = "participant")
dpy_melt$type <- as.factor(dpy_melt$type)
levels(dpy_melt$type)

##age
age_mod1 <- lm(dat$diff_20 ~ age, data = dat)
summary(age_mod1)
confint(age_mod1)

age_mod2 <- lm(dat$diff_born ~ age, data = dat)
summary(age_mod2)
confint(age_mod2)

####decline per year model###
dpy_mod <- lm(dpy ~ age, data = dpy_melt[dpy_melt$type == "dpy_total",])
summary(dpy_mod)
confint(dpy_mod)

#####covariates####
dat$political_ideology_n <- mapvalues(dat$political_ideology, from = c("Very liberal","Somewhat liberal","Neither liberal nor conservative",
                                                                       "Somewhat conservative","Very conservative"),
                                      to = c(-2,-1,0,1,2))
dat$political_ideology_n <- as.numeric(as.character(dat$political_ideology_n))

dat$education_n <- mapvalues(dat$education, from = c("High school diploma","Some college","Associate's degree",
                                                     "Four-year college degree","Some graduate school","Graduate school"),
                             to = c(1,2,3,4,5,6))
dat$education_n <- as.numeric(as.character(dat$education_n))

##dichotomize having kids
dat$kids_cat <- mapvalues(dat$kids, from = c("0","1","2","3","4+"),
                          to = c("no_kids","kids","kids","kids","kids"))
dat$kids_cat <- relevel(as.factor(dat$kids_cat), ref = "no_kids")

##big mod
big_mod <- lm(diff_born ~ political_ideology_n + age + race + gender + education + kids_cat, data = dat)
summary(big_mod)
confint(big_mod)

means <- emmeans(big_mod, specs = ~ race)
means
contr <- contrast(means, method = "pairwise", adjust = "holm")
contr
confint(contr)

##did liberals perceive decline too?
t.test(dat$diff_born[dat$political_ideology_n < 0])
cohensD(dat$diff_born[dat$political_ideology_n < 0])

####plot####
good_melt$time <- factor(good_melt$time, levels = c("born","twenty","today"))
plot <- ggplot(good_melt, aes(x = time, y = rating)) +
  stat_summary(fun.data = "mean_cl_boot") +
  coord_cartesian(ylim = c(4,5.5))

ggplot_build(plot)

####export for plot####
#write.csv(good_melt, file = "2c_melt.csv")

#####session info#####
#R version 4.0.2 (2020-06-22)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS  10.16

#Matrix products: default
#LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] dotwhisker_0.5.0 blme_1.0-4       emmeans_1.4.7    lsr_0.5          jtools_2.2.0     psych_1.9.12.31 
#[7] tidyr_1.1.0      lmerTest_3.1-2   lme4_1.1-23      Matrix_1.2-18    plyr_1.8.6       dplyr_1.0.7     
#[13] ggplot2_3.3.0   

#loaded via a namespace (and not attached):
#  [1] statmod_1.4.34      ggstance_0.3.4      tidyselect_1.1.0    xfun_0.14           purrr_0.3.4        
#[6] pander_0.6.5        splines_4.0.2       lattice_0.20-41     colorspace_1.4-1    vctrs_0.3.8        
#[11] generics_0.0.2      htmltools_0.5.0     yaml_2.2.1          utf8_1.1.4          rlang_0.4.11       
#[16] pillar_1.6.3        nloptr_1.2.2.1      glue_1.4.1          withr_2.2.0         DBI_1.1.0          
#[21] lifecycle_1.0.1     stringr_1.4.0       munsell_0.5.0       gtable_0.3.0        mvtnorm_1.1-0      
#[26] coda_0.19-3         evaluate_0.14       knitr_1.28          pbkrtest_0.4-8.6    parallel_4.0.2     
#[31] fansi_0.4.1         broom_0.5.6         Rcpp_1.0.4.6        xtable_1.8-4        backports_1.1.7    
#[36] scales_1.1.1        mnormt_1.5-7        packrat_0.7.0       digest_0.6.25       stringi_1.7.5      
#[41] numDeriv_2016.8-1.1 grid_4.0.2          tools_4.0.2         magrittr_1.5        tibble_3.0.1       
#[46] crayon_1.3.4        pkgconfig_2.0.3     ellipsis_0.3.2      MASS_7.3-51.6       estimability_1.3   
#[51] assertthat_0.2.1    minqa_1.2.4         rmarkdown_2.1       rstudioapi_0.11     R6_2.4.1           
#[56] boot_1.3-25         nlme_3.1-148        compiler_4.0.2 