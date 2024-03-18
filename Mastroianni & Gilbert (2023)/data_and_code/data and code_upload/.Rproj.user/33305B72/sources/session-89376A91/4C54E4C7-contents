##R code for analyses in Study 3 of "The Illusion of Moral Decline" (Mastroianni & Gilbert, 2023)

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
require(dotwhisker)
require(emmeans)

dat <- read.csv("study3.csv")
head(dat)
dim(dat)
set.seed(666)

#####exclusions#####
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

#####initial demographics#####
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

#####exclusions#####
##attention check
dat$inclusion[dat$check_3 != "Other"] <- "exclude"
dat$reason[dat$check_3 != "Other"] <- "check"
dat$inclusion[dat$check_3 == ""] <- "exclude"
table(dat$inclusion)
levels(as.factor(dat$check_3_4_TEXT))
dat <- dat[is.na(dat$inclusion),]

##age check
levels(dat$age_1)
dat$inclusion[dat$age_1 == "20-34" & (dat$age < 20 | dat$age > 34)] <- "exclude"
dat$inclusion[dat$age_1 == "35-49" & (dat$age < 35 | dat$age > 49)] <- "exclude"
dat$inclusion[dat$age_1 == "50-64" & (dat$age < 50 | dat$age > 64)] <- "exclude"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]

##manipuation check
dat$diff <- dat$today - dat$X2005

dat$diff_cat <- NULL
dat$diff_cat[dat$diff == 0] <- "equal"
dat$diff_cat[dat$diff > 0] <- "more"
dat$diff_cat[dat$diff < 0] <- "less"

dat$check
dat[dat$check == "4",]
levels(dat$check)
dat$cat2 <- mapvalues(dat$check, from = c("People are equally kind, honest, nice, and good today compared to about 15 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 15 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 15 years ago"),
                      to = c("equal","less","more"))

table(dat$cat2, dat$diff_cat)
dat$inclusion[dat$cat2 != dat$diff_cat] <- "exclude"
table(dat$inclusion)
dim(dat)

##add change and replacement effects
dat$change <- dat$middle_today - dat$middle_past
dat$replacement <- dat$replacements - dat$replaced

##reshape data
good_melt <- reshape(dat, varying = c("today","X2005"),
                     v.names = "rating", timevar = "time",
                     times = c("today","2005"),
                     direction = "long",
                     idvar = "participant")
good_melt$time <- as.factor(good_melt$time)

####exclude, re-report demographics####
good_melt <- good_melt[is.na(good_melt$inclusion),]
dat <- dat[is.na(dat$inclusion),]
dim(dat)
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

######moral decline#####
summary(dat$today)
summary(dat$X2005)
t.test(dat$today, dat$X2005, paired = TRUE)
cohensD(x = dat$today, y = dat$X2005, method = "paired")

#####what predicts decline#####
decline_mod <- lm(diff ~ change + replacement, data = dat)
summary(decline_mod)
confint(decline_mod)

#####covariates####
##dichotomize having kids
dat$kids_cat <- mapvalues(dat$kids, from = c("0","1","2","3","4+"),
                          to = c("no_kids","kids","kids","kids","kids"))
dat$kids_cat <- relevel(as.factor(dat$kids_cat), ref = "no_kids")

dat$political_ideology_n <- mapvalues(dat$political_ideology, from = c("Very liberal","Somewhat liberal","Neither liberal nor conservative",
                                                                       "Somewhat conservative","Very conservative"),
                                      to = c(-2,-1,0,1,2))
dat$political_ideology_n <- as.numeric(as.character(dat$political_ideology_n))

dat$education_n <- mapvalues(dat$education, from = c("High school diploma","Some college","Associate's degree",
                                                     "Four-year college degree","Some graduate school","Graduate school"),
                             to = c(1,2,3,4,5,6))
dat$education_n <- as.numeric(as.character(dat$education_n))

big_mod1 <- lm(diff ~ change + replacement + age + race + gender + education_n + kids_cat, data = dat)
summary(big_mod1)
confint(big_mod1)

####demographics####
big_mod <- lm(diff ~ political_ideology_n + age + race + gender + education_n + kids_cat, data = dat)
summary(big_mod)
confint(big_mod)

means <- emmeans(big_mod, specs = ~ race)
means
contr <- contrast(means, method = "pairwise", adjust = "holm")
contr
confint(contr)

####plot all means####
plot_melt <- reshape(dat, varying = c("today","X2005","middle_past","middle_today","replacements","replaced"),
                     v.names = "rating", timevar = "target", times = c("people in general today",
                                                                       "people in general 2005",
                                                                       "20-80 2005","35-95 today",
                                                                       "20-35 today","80-95 2005"),
                     direction = "long", idvar = "participant")

plot_melt$time <- mapvalues(plot_melt$target, from = c("people in general today",
                                                      "people in general 2005",
                                                      "20-80 2005","35-95 today",
                                                      "20-35 today","80-95 2005"),
                            to = c("today","2005","2005","today","today","2005"))
plot_melt$target2 <- mapvalues(plot_melt$target, from = c("people in general today",
                                                          "people in general 2005",
                                                          "20-80 2005","35-95 today",
                                                          "20-35 today","80-95 2005"),
                               to = c("People in general","People in general","20-80 year-olds","35-95 year-olds","20-35 year-olds","80-95 year-olds"))

means_plot <- ggplot(plot_melt, aes(x = time, y = rating, color = target2)) +
  geom_violin(position = position_dodge(width = .4)) +
  geom_point(position = position_jitterdodge(dodge.width = .4, jitter.height = .2), alpha = .1) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, geom = "errorbar", position = position_dodge(width = .4)) +
  stat_summary(fun = "mean", size = 1, position = position_dodge(width = .4)) +
  #stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = .4)) +
  coord_cartesian(ylim = c(1,7)) +
  ylab("Perceived Morality") +
  xlab("Time") +
  theme(legend.title = element_blank())

means_mod <- lmer(rating ~ target + (1|participant), data = plot_melt)
summary(means_mod)
means <- emmeans(means_mod, specs = ~ target)
means
contr <- contrast(means, method = "pairwise", adjust = "holm")
contr
confint(contr)

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

theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}


all_plot_black <- ggplot(good_melt, aes(x = year, y = rating)) +
  geom_violin(adjust = .5, fill = "black", trim = "white") + 
  geom_point(position = position_jitter(height = .2, width = .2), color = "gray", alpha = .1) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, geom = "errorbar", color = "white", width = .5) +
  stat_summary(fun = "mean", size = 2, color = "white") +
  ylab(NULL) +
  #scale_x_discrete(labels = c("40y Before Birth","20y Before Birth","Year of Birth",
  #                           "20y After Birth","Today")) +
  xlab(label = NULL) +
  theme_black(base_size = 32) +
  coord_cartesian(ylim = c(1,7))
