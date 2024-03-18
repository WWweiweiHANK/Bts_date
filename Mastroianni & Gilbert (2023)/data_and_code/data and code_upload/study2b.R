##R code for analyses in Study 2b of "The Illusion of Moral Decline" (Mastroianni & Gilbert, 2023)

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
require(psych)
require(emmeans)

dat <- read.csv("study2b.csv")
head(dat)
dim(dat)


#####data cleaning, remove people who didn't finish#####
dat$inclusion <- NULL
dat$reason <- NULL

dat$inclusion[dat$Finished == "FALSE"] <- "exclude"
dat$reason[dat$Finished == "FALSE"] <- "incomplete"

dat <- dat[is.na(dat$inclusion),]
dim(dat)

dat$zip <- as.factor(dat$zip)

dat$inclusion[dat$kindergarten != "Kindergarten"] <- "exclude"
dat$inclusion[dat$zip != "2138"] <- "exclude"
dat$inclusion[dat$turkey != "Eating turkey"] <- "exclude"
dat$reason[dat$inclusion == "exclude"] <- "english"
table(dat$inclusion)

dat <- dat[is.na(dat$inclusion),]
dim(dat)

#####report initial demographics######
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

#####exclusions######
dat$inclusion[dat$check != "Other"] <- "exclude"
dat$reason[dat$check != "Other"] <- "check"
levels(as.factor(dat$check_4_TEXT))
dat$inclusion[dat$check_4_TEXT == ""] <- "exclude"
dat$inclusion[dat$check_4_TEXT == "word"] <- "exclude"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]

dim(dat)

#####re-report demographics#####
dim(dat)
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

#####data cleaning#####
dat$today <- mapvalues(dat$today, from = c("7 (very)","1 (not very)"), to = c("7","1"))
dat$X2018 <- mapvalues(dat$X2018, from = c("7 (very)","1 (not very)"), to = c("7","1"))
dat$X2016 <- mapvalues(dat$X2016, from = c("7 (very)","1 (not very)"), to = c("7","1"))
dat$X2014 <- mapvalues(dat$X2014, from = c("7 (very)","1 (not very)"), to = c("7","1"))
dat$X2012 <- mapvalues(dat$X2012, from = c("7 (very)","1 (not very)"), to = c("7","1"))
dat$X2010 <- mapvalues(dat$X2010, from = c("7 (very)","1 (not very)"), to = c("7","1"))

dat$today <- as.integer(as.character(dat$today))
dat$X2018 <- as.integer(as.character(dat$X2018))
dat$X2016 <- as.integer(as.character(dat$X2016))
dat$X2014 <- as.integer(as.character(dat$X2014))
dat$X2012 <- as.integer(as.character(dat$X2012))
dat$X2010 <- as.integer(as.character(dat$X2010))

#####quality check#####
table(dat$manip_check)
dat$ten_trend <- dat$today - dat$X2010
summary(dat$ten_trend)
describeBy(dat$ten_trend, dat$manip_check)
dat$trend <- NULL
dat$trend[dat$ten_trend == 0] <- "equal"
dat$trend[dat$ten_trend > 0] <- "more"
dat$trend[dat$ten_trend < 0] <- "less"

table(dat$manip_check, dat$trend)

dat$strict <- NULL
dat$strict[dat$ten_trend < 0 & dat$manip_check !=  "Less kind, honest, nice, good today compared to ten years ago"] <- "exclude"
dat$strict[dat$ten_trend == 0 & dat$manip_check != "Equally kind, honest, nice, good today and ten years ago"] <- "exclude"
dat$strict[dat$ten_trend > 0 & dat$manip_check != "More kind, honest, nice, good today compared to ten years ago"] <- "exclude"

table(dat$strict)
dat <- dat[is.na(dat$strict),]
dim(dat)

#####re-report demographics#####
dim(dat)
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

#####reshape#####
dat$diff <- dat$today - dat$X2010
good_melt <- reshape(dat, varying = c("today","X2018","X2016","X2014","X2012","X2010"),
                     v.names = "rating", timevar = "year", times = c("2020","2018","2016","2014","2012","2010"),
                     direction = "long", idvar = "participant")

#####model#####
good_melt$year <- as.factor(good_melt$year)
good_melt$year <- factor(good_melt$year, levels(good_melt$year)[c(6,5,4,3,2,1)])
good_mod <- lmer(rating ~ year + (1|participant), data = good_melt)
summary(good_mod)

means <- emmeans(good_mod, specs = ~ year)
means
eff_size(means, sigma = sigma(good_mod), edf = 735)
contr <- contrast(means, method = "pairwise", adjust = "holm")
contr
confint(contr)

####check model###
plot(good_mod, type = c("p", "smooth"), col.line = "black")
hist(good_melt$rating)
plot(good_mod)
qqline(resid(good_mod))

#####covariates####

##dichotomize having kids
dat$kids_cat <- mapvalues(dat$kids, from = c("0","1","2","3","4+"),
                          to = c("no_kids","kids","kids","kids","kids"))

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

##are any race effects significant?
means <- emmeans(big_mod, specs = ~ race)
means
contr <- contrast(means, method = "pairwise", adjust = "holm")
contr
confint(contr)
#no

####plot####
good_melt$year <- factor(good_melt$year, levels = c("2010","2012","2014","2016","2018","2020"))
plot <- ggplot(good_melt, aes(x = year, y = rating)) +
  stat_summary(fun.data = "mean_cl_boot") +
  coord_cartesian(ylim = c(4,5.5))

ggplot_build(plot)

####export for plot####
#write.csv(good_melt, file = "2b_melt.csv")

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
  geom_point(position = position_jitter(height = .2, width = .2), color = "gray", alpha = .1) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, geom = "errorbar", color = "white", width = .5) +
  stat_summary(fun = "mean", size = 2, color = "white") +
  ylab(NULL) +
  #scale_x_discrete(labels = c("40y Before Birth","20y Before Birth","Year of Birth",
  #                           "20y After Birth","Today")) +
  xlab(label = NULL) +
  theme_black(base_size = 32) +
  coord_cartesian(ylim = c(1,7))

####sessioninfo####

#R version 4.0.2 (2020-06-22)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: OS X Lion 11.7.4

#Matrix products: default
#LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] parameters_0.20.2 dotwhisker_0.5.0  tidyr_1.1.0       lsr_0.5           blme_1.0-4        emmeans_1.4.7    
#[7] psych_1.9.12.31   lmerTest_3.1-2    lme4_1.1-23       Matrix_1.2-18     plyr_1.8.6        dplyr_1.0.7      
#[13] ggplot2_3.3.0     jtools_2.2.0     

#loaded via a namespace (and not attached):
#  [1] splines_4.0.2       Formula_1.2-3       datawizard_0.6.5    assertthat_0.2.1    statmod_1.4.34     
#[6] latticeExtra_0.6-29 pander_0.6.5        ggstance_0.3.4      bayestestR_0.13.0   numDeriv_2016.8-1.1
#[11] pillar_1.6.3        backports_1.1.7     lattice_0.20-41     glue_1.4.1          digest_0.6.25      
#[16] RColorBrewer_1.1-2  checkmate_2.0.0     minqa_1.2.4         colorspace_1.4-1    htmltools_0.5.0    
#[21] pkgconfig_2.0.3     broom_0.5.6         purrr_0.3.4         xtable_1.8-4        mvtnorm_1.1-0      
#[26] scales_1.1.1        jpeg_0.1-8.1        tibble_3.0.1        htmlTable_1.13.3    farver_2.0.3       
#[31] generics_0.0.2      ellipsis_0.3.2      withr_2.2.0         nnet_7.3-14         pbkrtest_0.4-8.6   
#[36] mnormt_1.5-7        survival_3.1-12     magrittr_1.5        crayon_1.3.4        estimability_1.3   
#[41] fansi_0.4.1         nlme_3.1-148        MASS_7.3-51.6       foreign_0.8-80      data.table_1.12.8  
#[46] tools_4.0.2         lifecycle_1.0.1     stringr_1.4.0       munsell_0.5.0       cluster_2.1.0      
#[51] compiler_4.0.2      rlang_0.4.11        grid_4.0.2          nloptr_1.2.2.1      rstudioapi_0.11    
#[56] htmlwidgets_1.5.1   labeling_0.3        base64enc_0.1-3     boot_1.3-25         gtable_0.3.0       
#[61] DBI_1.1.0           R6_2.4.1            gridExtra_2.3       knitr_1.28          utf8_1.1.4         
#[66] Hmisc_4.4-0         insight_0.19.0      stringi_1.7.5       parallel_4.0.2      Rcpp_1.0.4.6       
#[71] vctrs_0.3.8         rpart_4.1-15        acepack_1.4.1       png_0.1-7           xfun_0.14          
#[76] tidyselect_1.1.0    coda_0.19-3    
