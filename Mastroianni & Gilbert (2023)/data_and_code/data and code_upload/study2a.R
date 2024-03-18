##R code for analyses in Study 2a of "The Illusion of Moral Decline" (Mastroianni & Gilbert 2023)

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

require(jtools)
require(ggplot2)
require(dplyr)
require(plyr)
require(lme4)
require(lmerTest)
require(psych)
require(emmeans)
require(blme)
require(lsr)

##read in data
dat <- read.csv("study2a.csv")
dim(dat)
head(dat)

#####clean data, remove any incompletes#####
dat$inclusion <- NULL
dat$reason <- NULL

dat$inclusion[dat$Finished == "FALSE"] <- "exclude"
dat$reason[dat$Finished == "FALSE"] <- "incomplete"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]

#####participant demographics#####
dim(dat)
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

#####exclusions#####
dat$inclusion[dat$check != "Other"] <- "exclude"
dat$reason[dat$check != "Other"] <- "check"
table(dat$inclusion)
levels(as.factor(dat$check_4_TEXT))
dat$inclusion[dat$check_4_TEXT == ""] <- "exclude"
table(dat$inclusion)
dat$inclusion[dat$check_4_TEXT == "Ambivert "] <- "exclude"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]
dim(dat)

##consistency check
table(dat$manip_check)
dat$trend <- dat$today - dat$X2000
summary(dat$trend)
describeBy(dat$trend, dat$manip_check)
dat$trend_cat <- NULL
dat$trend_cat[dat$trend == 0] <- "equal"
dat$trend_cat[dat$trend > 0] <- "more"
dat$trend_cat[dat$trend < 0] <- "less"

dat$manip_check <- as.factor(dat$manip_check)
levels(dat$manip_check)
dat$strict <- NULL
dat$strict[dat$trend < 0 & dat$manip_check !=  "I said that people today are LESS kind, honest, nice, and good today than people were 20 years ago"] <- "exclude"
dat$strict[dat$trend == 0 & dat$manip_check != "I said that people today are EQUALLY kind, honest, nice, and good today as people were 20 years ago"] <- "exclude"
dat$strict[dat$trend > 0 & dat$manip_check != "I said that people today are MORE kind, honest, nice, and good today than people were 20 years ago"] <- "exclude"
table(dat$strict)

#####exclude quality check exclusions, re-report demos#####
dat <- dat[is.na(dat$strict),]
dim(dat)
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

##reshape data
good_melt <- reshape(dat, varying = c("today","X2010","X2000"),
                     v.names = "rating", timevar = "year", times = c("2020","2010","2000"),
                     direction = "long", idvar = "participant")

good_melt <- good_melt[is.na(good_melt$strict),]

#####main analysis#####
good_mod <- lmer(rating ~ year + (1|participant), data = good_melt)
summary(good_mod)

means <- emmeans(good_mod, specs = ~ year)
means
eff_size(means, edf = 1394, sigma = sigma(good_mod))
contr <- contrast(means, method = "pairwise", adjust = "holm")
contr
confint(contr)

####check model###
plot(good_mod, type = c("p", "smooth"), col.line = "black")
hist(good_melt$rating)
plot(good_mod)
qqline(resid(good_mod))

#####covariates####
dat$diff <- dat$today - dat$X2000

##dichotomize having kids
dat$kids_cat <- mapvalues(dat$kids, from = c("0","1","2","3","4+"),
                          to = c("no_kids","kids","kids","kids","kids"))
dat$kids_cat <- relevel(as.factor(dat$kids_cat), ref = "no_kids")

##make ideology continuous
dat$political_ideology_n <- mapvalues(dat$political_ideology, from = c("Very liberal","Somewhat liberal","Neither liberal nor conservative",
                                                                       "Somewhat conservative","Very conservative"),
                                      to = c(-2,-1,0,1,2))
dat$political_ideology_n <- as.numeric(as.character(dat$political_ideology_n))

##make education continuous
dat$education_n <- mapvalues(dat$education, from = c("High school diploma","Some college","Associate's degree",
                                                     "Four-year college degree","Some graduate school","Graduate school"),
                             to = c(1,2,3,4,5,6))
dat$education_n <- as.numeric(as.character(dat$education_n))

#fit model
big_mod <- lm(diff ~ political_ideology_n + age + race + gender + education_n + kids_cat, data = dat)
summary(big_mod)
confint(big_mod)

means <- emmeans(big_mod, specs = ~ race)
means
contr <- contrast(means, method = "pairwise", adjust = "holm")
contr
confint(contr)

##perception across political spectrum
t.test(dat$diff[dat$political_ideology == "Very liberal" | dat$political_ideology == "Somewhat liberal"])
cohensD(dat$diff[dat$political_ideology == "Very liberal" | dat$political_ideology == "Somewhat liberal"])

####plot####
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 500)

plot <- ggplot(good_melt, aes(x = year, y = rating)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1.5) +
  stat_summary(fun.data = "mean_cl_boot", size = 1.5, geom = "errorbar") +
  coord_cartesian(ylim = c(4,5.5)) +
  xlab("Year") +
  ylab("Perceived Morality") +
  theme_apa() +
  scale_x_discrete(expand = c(.0, .0)) +
  theme(panel.grid.major.y = element_line(color = "gray",
                                          size = 0.5),
        panel.grid.minor.y = element_line(color = col_grid))

#####export for plot####
#write.csv(good_melt, file = "2a_melt.csv")

####session info#####
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
#  [1] blme_1.0-4      emmeans_1.4.7   lsr_0.5         jtools_2.2.0    psych_1.9.12.31 tidyr_1.1.0    
#[7] lmerTest_3.1-2  lme4_1.1-23     Matrix_1.2-18   plyr_1.8.6      dplyr_1.0.7     ggplot2_3.3.0  

#loaded via a namespace (and not attached):
#  [1] statmod_1.4.34      tidyselect_1.1.0    xfun_0.14           purrr_0.3.4         pander_0.6.5       
#[6] splines_4.0.2       lattice_0.20-41     colorspace_1.4-1    vctrs_0.3.8         generics_0.0.2     
#[11] htmltools_0.5.0     yaml_2.2.1          utf8_1.1.4          rlang_0.4.11        pillar_1.6.3       
#[16] nloptr_1.2.2.1      glue_1.4.1          withr_2.2.0         DBI_1.1.0           lifecycle_1.0.1    
#[21] munsell_0.5.0       gtable_0.3.0        mvtnorm_1.1-0       coda_0.19-3         evaluate_0.14      
#[26] knitr_1.28          pbkrtest_0.4-8.6    parallel_4.0.2      fansi_0.4.1         Rcpp_1.0.4.6       
#[31] xtable_1.8-4        scales_1.1.1        mnormt_1.5-7        packrat_0.7.0       digest_0.6.25      
#[36] numDeriv_2016.8-1.1 grid_4.0.2          tools_4.0.2         magrittr_1.5        tibble_3.0.1       
#[41] crayon_1.3.4        pkgconfig_2.0.3     ellipsis_0.3.2      MASS_7.3-51.6       estimability_1.3   
#[46] assertthat_0.2.1    minqa_1.2.4         rmarkdown_2.1       rstudioapi_0.11     R6_2.4.1           
#[51] boot_1.3-25         nlme_3.1-148        compiler_4.0.2
