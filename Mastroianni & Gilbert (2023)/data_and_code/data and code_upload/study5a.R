##R code for analyses in Study 5a of "The Illusion of Moral Decline" (Mastroianni & Gilbert, 2023)

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
require(jtools)
require(lsr)
require(emmeans)

dat <- read.csv("study5a.csv")
head(dat)
dim(dat)

#####data cleaning, remove incompletes#####
dat$inclusion <- NULL
dat$reason <- NULL

dat$inclusion[dat$Finished == "FALSE"] <- "exclude"
dat$reason[dat$Finished == "FALSE"] <- "incomplete"
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

#####report initial demographics#####
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)), 2)

#####exclusions#####
##attention 1
dat$inclusion[!is.na(dat$final_check)] <- "exclude"
table(dat$inclusion)

dat <- dat[is.na(dat$inclusion),]

##attention check
dat$inclusion[dat$check_3 != "Other"] <- "exclude"
dat$reason[dat$check_3 != "Other"] <- "check"
table(dat$inclusion)
dat$inclusion[dat$check_3_4_TEXT == "both"] <- "exclude"
dat$inclusion[dat$check_3_4_TEXT == "Both, depends on situation"] <- "exclude"
dat$inclusion[dat$check_3_4_TEXT == "shor"] <- "exclude"
dat$inclusion[dat$check_3_4_TEXT == "shoes"] <- "exclude"
dat <- dat[is.na(dat$inclusion),]
dim(dat)

##year check
dat$birth_year <- as.numeric(as.character(dat$birth_year))
dat$year_age <- 2020 - dat$birth_year
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

#####re-report demographics after exclusions#####
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

#####model#####
##moral decline
t.test(dat$overall)
cohensD(dat$overall)

t.test(dat$all_change)
cohensD(dat$all_change)

t.test(dat$all_replacement)
cohensD(dat$all_replacement)

t.test(dat$known_change)
cohensD(dat$known_change)

t.test(dat$known_replacement)
cohensD(dat$known_replacement)

t.test(dat$all_replacement, dat$known_replacement, paired = T)
cohensD(dat$all_replacement, dat$known_replacement)

#####covariates#####
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
big_mod <- lm(overall ~ political_ideology_n + age + race + gender + education_n + kids_cat, data = dat)
summary(big_mod)
confint(big_mod)

means <- emmeans(big_mod, specs = ~ race)
means
contr <- contrast(means, method = "pairwise", adjust = "holm")
contr
confint(contr)

##predicting overall moral decline
mod1 <- lm(overall ~ all_change + all_replacement, data = dat[dat$known_in_check == "No",])
summary(mod1)
dim(dat[dat$known_in_check == "No",])

mod2 <- lm(overall ~ all_change + all_replacement + known_change, data = dat[dat$known_in_check != "No",])
summary(mod2)
dim(dat[dat$known_in_check != "No",])

mod3 <- lm(overall ~ all_change + all_replacement + known_change + known_replacement, data = dat[(dat$replacements_yes != "No" | dat$replacements_yes.1 != "No") & dat$known_in_check != "No",])
summary(mod3)
dim(dat[!is.na(dat$known_replacement) & !is.na(dat$known_change),])

dim(dat[!is.na(dat$known_replacement) & is.na(dat$known_change),])

####plot####
all_melt <- reshape(dat, varying = c("overall", "all_change", "all_replacement", "known_change", "known_replacement"),
                    v.names = "rating", timevar = "type", times = c("overall", "all_change","all_replacement","known_change","known_replacement"),
                    direction = "long", idvar = "participant")
table(all_melt$type)
all_melt$type <- factor(all_melt$type, levels = c("overall","all_change","all_replacement","known_change","known_replacement"))
all_plot <- ggplot(all_melt, aes(x = type, y = rating)) +
  geom_violin() +
  geom_point(position = position_jitter(height = .2, width = .2), color = "gray", alpha = .2) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, geom = "errorbar") +
  stat_summary(fun = "mean", size = 1) +
  ylab("Perceived Moral Change") +
  scale_x_discrete(labels = c("Overall","Personal Change\n(People in General)","Interpersonal Replacement\n(People in General)",
                              "Personal Change\n(Personal World)","Interpersonal Replacement\n(Personal World)")) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  xlab(label = NULL) +
  theme_apa() +
  theme(text=element_text(family="Arial"))

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
#  [1] emmeans_1.4.7   lsr_0.5         jtools_2.2.0    psych_1.9.12.31 tidyr_1.1.0     lmerTest_3.1-2 
#[7] lme4_1.1-23     Matrix_1.2-18   plyr_1.8.6      dplyr_1.0.7     ggplot2_3.3.0  

#loaded via a namespace (and not attached):
#  [1] statmod_1.4.34      tidyselect_1.1.0    xfun_0.14           purrr_0.3.4         pander_0.6.5       
#[6] splines_4.0.2       lattice_0.20-41     colorspace_1.4-1    vctrs_0.3.8         generics_0.0.2     
#[11] htmltools_0.5.0     yaml_2.2.1          utf8_1.1.4          rlang_0.4.11        pillar_1.6.3       
#[16] nloptr_1.2.2.1      glue_1.4.1          withr_2.2.0         DBI_1.1.0           lifecycle_1.0.1    
#[21] munsell_0.5.0       gtable_0.3.0        mvtnorm_1.1-0       coda_0.19-3         evaluate_0.14      
#[26] knitr_1.28          parallel_4.0.2      fansi_0.4.1         Rcpp_1.0.4.6        xtable_1.8-4       
#[31] scales_1.1.1        mnormt_1.5-7        packrat_0.7.0       digest_0.6.25       numDeriv_2016.8-1.1
#[36] grid_4.0.2          tools_4.0.2         magrittr_1.5        tibble_3.0.1        crayon_1.3.4       
#[41] pkgconfig_2.0.3     ellipsis_0.3.2      MASS_7.3-51.6       estimability_1.3    assertthat_0.2.1   
#[46] minqa_1.2.4         rmarkdown_2.1       rstudioapi_0.11     R6_2.4.1            boot_1.3-25        
#[51] nlme_3.1-148        compiler_4.0.2  

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

all_plot_black <- ggplot(all_melt, aes(x = type, y = rating)) +
  #geom_point(position = position_jitter(height = .2, width = .2), color = "gray", alpha = .3) +
  stat_summary(fun.data = "mean_cl_boot", size = 3, color = "white") +
  #stat_summary(fun = "mean", size = 2, color = "white") +
  ylab("moral change") +
  scale_x_discrete(labels = c("Overall","Individual Change\n(People in General)","Cohort Replacement\n(People in General)",
                              "Individual Change\n(Personal World)","Cohort Replacement\n(Personal World)")) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  xlab(label = NULL) +
  theme_black(base_size = 26)
