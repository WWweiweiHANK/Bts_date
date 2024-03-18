##R code for analyses in Study S2 "The Illusion of Moral Decline" (Mastroianni & Gilbert, 2023)

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
require(emmeans)

dat <- read.csv("studyS2.csv")
dim(dat)
head(dat)

##remove incompletes
dat$inclusion <- NULL
dat$reason <- NULL

dat$inclusion[dat$Finished == "FALSE"] <- "exclude"
dat$reason[dat$Finished == "FALSE"] <- "incomplete"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]

##remove english test failures (these people gave us no data)
dat$zip <- as.factor(dat$zip)

dat$inclusion[dat$kindergarten != "Kindergarten"] <- "exclude"
dat$inclusion[dat$zip != "2138"] <- "exclude"
dat$inclusion[dat$turkey != "Eating turkey"] <- "exclude"
dat$reason[dat$inclusion == "exclude"] <- "english"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]

#####participant demographics#####
dim(dat)
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

#####data cleaning#####
dat$today <- mapvalues(dat$today, from = c("7 (very)","1 (not very)"), to = c("7","1"))
dat$X2010 <- mapvalues(dat$X2010, from = c("7 (very)","1 (not very)"), to = c("7","1"))
dat$X2000 <- mapvalues(dat$X2000, from = c("7 (very)","1 (not very)"), to = c("7","1"))
dat$X1990 <- mapvalues(dat$X1990, from = c("7 (very)","1 (not very)"), to = c("7","1"))
dat$today <- as.integer(as.character(dat$today))
dat$X2010 <- as.integer(as.character(dat$X2010))
dat$X2000 <- as.integer(as.character(dat$X2000))
dat$X1990 <- as.integer(as.character(dat$X1990))

#####EXCLUSIONS#####
dat$inclusion[dat$check != "Other"] <- "exclude"
dat$reason[dat$check != "Other"] <- "check"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]
levels(as.factor(dat$check_4_TEXT))
dat$inclusion[dat$check_4_TEXT == ""] <- "exclude"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]
dim(dat)

##one person says they are 300
dat <- dat[dat$age < 300,]

#####strict exclusion#####
table(dat$manip_check)
dat$trend <- dat$today - dat$X1990
summary(dat$trend)
describeBy(dat$trend, dat$manip_check)
dat$trend_cat <- NULL
dat$trend_cat[dat$trend == 0] <- "equal"
dat$trend_cat[dat$trend > 0] <- "more"
dat$trend_cat[dat$trend < 0] <- "less"

table(dat$manip_check, dat$trend_cat)

levels(dat$manip_check)
dat$strict <- NULL
dat$strict[dat$trend < 0 & dat$manip_check !=  "People are LESS kind, honest, nice, and good today compared to thirty years ago"] <- "exclude"
dat$strict[dat$trend == 0 & dat$manip_check != "People are equally kind, honest, nice, and good today compared to thirty years ago"] <- "exclude"
dat$strict[dat$trend > 0 & dat$manip_check != "People are MORE kind, honest, nice, and good today compared to thirty years ago"] <- "exclude"

table(dat$strict)

#####check excluded#####
dim(dat)
good_melt <- reshape(dat, varying = c("today","X2010","X2000","X1990"),
                     v.names = "rating", timevar = "year", times = c("2020","2010","2000","1990"),
                     direction = "long", idvar = "participant")
head(good_melt)
class(good_melt)

good_plot_compare <- ggplot(good_melt, aes(x = year, y = rating, color = strict)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = .2)) +
  ggtitle("Study 1 redux comparing people who fail strict manipulation check (N = 53) to those who don't (N = 135)")

#####exclude strict exclusions, re-report demos#####
dat <- dat[is.na(dat$strict),]
dim(dat)
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

good_melt <- good_melt[is.na(good_melt$strict),]

#####model#####
good_melt$year <- as.factor(good_melt$year)
good_melt$year <- factor(good_melt$year, levels = c("2020", "2010","2000","1990"))
good_mod <- lmer(rating ~ year + (1|participant), data = good_melt)
summary(good_mod)

means <- emmeans(good_mod, specs = ~ year)
means
contr <- contrast(means, method = "pairwise", adjust = "holm")
contr
confint(contr)

#####age analysis#####
dat$decline <- dat$today - dat$X1990
age_mod <- lm(decline ~ age, data = dat)
summary(age_mod)
##no age effect

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
#  [1] lmerTest_3.1-2 lme4_1.1-23    Matrix_1.2-18  plyr_1.8.6     dplyr_1.0.7    ggplot2_3.3.0 

#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.4.6        nloptr_1.2.2.1      pillar_1.6.3        compiler_4.0.2      tools_4.0.2        
#[6] boot_1.3-25         statmod_1.4.34      digest_0.6.25       packrat_0.7.0       nlme_3.1-148       
#[11] evaluate_0.14       lifecycle_1.0.1     tibble_3.0.1        gtable_0.3.0        lattice_0.20-41    
#[16] pkgconfig_2.0.3     rlang_0.4.11        DBI_1.1.0           rstudioapi_0.11     yaml_2.2.1         
#[21] xfun_0.14           withr_2.2.0         knitr_1.28          generics_0.0.2      vctrs_0.3.8        
#[26] grid_4.0.2          tidyselect_1.1.0    glue_1.4.1          R6_2.4.1            fansi_0.4.1        
#[31] rmarkdown_2.1       minqa_1.2.4         purrr_0.3.4         magrittr_1.5        MASS_7.3-51.6      
#[36] splines_4.0.2       scales_1.1.1        ellipsis_0.3.2      htmltools_0.5.0     assertthat_0.2.1   
#[41] colorspace_1.4-1    numDeriv_2016.8-1.1 utf8_1.1.4          munsell_0.5.0       crayon_1.3.4 
