##Code for reproducing analyses in Study S3 in "The Illusion of Moral Decline" (Mastroianni & Gilbert, 2023)

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

require(ggplot2)
require(dplyr)
require(plyr)
require(lme4)
require(lmerTest)
require(tidyr)
require(psych)

dat <- read.csv("studyS3.csv")
head(dat)
dim(dat)

#####data cleaning, remove incompletes#####
dat$inclusion <- NULL
dat$reason <- NULL

dat$inclusion[dat$Finished == "FALSE"] <- "exclude"
dat$reason[dat$Finished == "FALSE"] <- "incomplete"
table(dat$inclusion)

dat <- dat[is.na(dat$inclusion),]

#####report initial demographics#####
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)), 2)

##english test
dat$inclusion[dat$footwear != "Bell bottoms"] <- "exclude"
dat$inclusion[dat$rsvp != "A wedding invitation"] <- "exclude"
dat$inclusion[dat$elevator != "An elevator"] <- "exclude"
dat$reason[dat$inclusion == "exclude"] <- "english"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]
dim(dat)
names(dat)
#####exclusions#####
##comprehension checks
dat$inclusion[dat$check_1 != "The person who chose to COOPERATE got nothing"] <- "exclude"
dat$inclusion[dat$check_2 != "They both got $30"] <- "exclude"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]


dat$inclusion[dat$check_3 != "Other"] <- "exclude"
dat$inclusion[dat$check_3_4_TEXT == ""] <- "exclude"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]


##year check
dat$birth_year <- as.numeric(as.character(dat$birth_year))
dat$year_age <- 2022 - dat$birth_year
dat$age_check <- NULL
for(i in 1:nrow(dat)){
  if((dat$year_age[i] - dat$age[i]) > 1 | (dat$year_age[i] - dat$age[i]) < 0){
    dat$age_check[i] <- "fail"
  } else{dat$age_check[i] <- "pass"}
}
dat$year_age - dat$age
table(dat$age_check)
dat$inclusion[dat$age_check == "fail"] <- "exclude"
dat$reason[dat$age_check == "fail"] <- "age_check"

dat <- dat[dat$age_check == "pass",]
dim(dat)

##trend
dat$trend <- dat$estimate_2017 - dat$estimate_1956
#check trend against choice
dat$inclusion[dat$trend > 0 & dat$choice != "I think the percentage of people who chose to cooperate went up between 1956 and 2017"] <- "exclude"
dat$inclusion[dat$trend == 0 & dat$choice != "I think the percentage of people who chose to cooperate stayed the same between 1956 and 2017"] <- "exclude"
dat$inclusion[dat$trend < 0 & dat$choice != "I think the percentage of people who chose to cooperate went down between 1956 and 2017"] <- "exclude"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]

##demographics of included sample
dim(dat)
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)), 2)

####analysis####
prop.table(table(dat$choice))
chisq.test(table(dat$choice))

summary(dat$estimate_1956)
summary(dat$estimate_2017)
t.test(dat$estimate_2017, dat$estimate_1956, paired = TRUE)

##session info
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
#  [1] tidyr_1.1.0     blme_1.0-4      emmeans_1.4.7   psych_1.9.12.31 lmerTest_3.1-2  lme4_1.1-23    
#[7] Matrix_1.2-18   plyr_1.8.6      dplyr_1.0.7     ggplot2_3.3.0   jtools_2.2.0   

#loaded via a namespace (and not attached):
#  [1] statmod_1.4.34      tidyselect_1.1.0    xfun_0.14           purrr_0.3.4         pander_0.6.5       
#[6] splines_4.0.2       lattice_0.20-41     colorspace_1.4-1    vctrs_0.3.8         generics_0.0.2     
#[11] htmltools_0.5.0     yaml_2.2.1          utf8_1.1.4          rlang_0.4.11        pillar_1.6.3       
#[16] nloptr_1.2.2.1      glue_1.4.1          withr_2.2.0         DBI_1.1.0           lifecycle_1.0.1    
#[21] lsr_0.5             munsell_0.5.0       gtable_0.3.0        mvtnorm_1.1-0       coda_0.19-3        
#[26] evaluate_0.14       knitr_1.28          parallel_4.0.2      fansi_0.4.1         Rcpp_1.0.4.6       
#[31] xtable_1.8-4        scales_1.1.1        mnormt_1.5-7        packrat_0.7.0       digest_0.6.25      
#[36] numDeriv_2016.8-1.1 grid_4.0.2          tools_4.0.2         magrittr_1.5        tibble_3.0.1       
#[41] crayon_1.3.4        pkgconfig_2.0.3     ellipsis_0.3.2      MASS_7.3-51.6       estimability_1.3   
#[46] assertthat_0.2.1    minqa_1.2.4         rmarkdown_2.1       rstudioapi_0.11     R6_2.4.1           
#[51] boot_1.3-25         nlme_3.1-148        compiler_4.0.2     

dat$change <- dat$estimate_2017-dat$estimate_1956
dat$actual <- rep(8, times = nrow(dat))
change_melt <- reshape(dat, varying = c("change","actual"),
                       v.names = "change", timevar = "type",
                       times = c("estimated","actual (Yuan et al., 2022)"),
                       idvar = "participant",
                       direction ="long")

ggplot(change_melt, aes(x = type, y = change, color = type)) +
  stat_summary(fun.data = "mean_cl_boot", color = "white", size = 3) +
  coord_cartesian(ylim = c(-25,25)) +
  xlab(NULL) +
  ylab("Change in % Cooperating") +
  theme_black(base_size = 36) +
  geom_hline(yintercept = 0, color = "red", size = 2)
> 