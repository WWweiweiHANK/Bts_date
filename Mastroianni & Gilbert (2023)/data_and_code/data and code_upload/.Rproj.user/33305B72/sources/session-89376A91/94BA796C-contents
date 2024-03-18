##R code for analyses in Study 5b of "The Illusion of Moral Decline" (Mastroianni & Gilbert, 2023)

#####IMPORTANT NOTES#####
##DO NOT open this code file by dragging it into a pre-existing R session.
##Instead, open the .rproj file
##This project is meant to be self-contained, and packages will install and load as necessary.
##However, it will not work unless opened via the .rproj file.
##Session info is available at the end of this document.
##if you encounter any issues, please email Adam Mastroianni at adam.m.mastroianni@gmail.com

addTaskCallback(function(...) {set.seed(123);TRUE})

require(ggplot2)
require(dplyr)
require(plyr)
require(lme4)
require(lmerTest)
require(tidyr)
require(emmeans)
require(jtools)
require(lsr)

dat <- read.csv("study5b.csv")
head(dat)
dim(dat)

#####data cleaning#####
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

####exclusions####
##english test
dat$inclusion[dat$footwear != "Bell bottoms"] <- "exclude"
dat$inclusion[dat$rsvp != "A wedding invitation"] <- "exclude"
dat$inclusion[dat$elevator != "An elevator"] <- "exclude"
dat$reason[dat$inclusion == "exclude"] <- "english"
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]

#####report initial demographics####
dim(dat)
table(dat$gender)
summary(dat$age)
round(prop.table(table(dat$race)),2)

####exclusions####
##attention check
dat$inclusion[dat$check != "Other"] <- "exclude"
dat$reason[dat$check != "Other"] <- "check"
dat$inclusion[dat$check_4_TEXT == ""] <- "exclude"
table(dat$check_4_TEXT)
table(dat$inclusion)
dat <- dat[is.na(dat$inclusion),]

##age check
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
dat$age[!is.na(dat$inclusion)]
dat$age_1[!is.na(dat$inclusion)]
##yeah generally these don't look great
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
                       dat$X1985, dat$X1980, dat$X1975)
summary(dat$twenty)

dat$before20 <- coalesce(dat$X1980_before,
                         dat$X1975_before,
                         dat$X1970_before,
                         dat$X1965_before,
                         dat$X1960_before,
                         dat$X1955_before,
                         dat$X1950_before,
                         dat$X1945_before,
                         dat$X1940_before,
                         dat$X1935_before)
summary(dat$before20)

dat$before40 <- coalesce(dat$X1960_before2,
                         dat$X1955_before2,
                         dat$X1950_before2,
                         dat$X1945_before2,
                         dat$X1940_before2,
                         dat$X1935_before2,
                         dat$X1930_before2,
                         dat$X1925_before2,
                         dat$X1920_before2,
                         dat$X1915_before2)
summary(dat$before40)

##adding effects
dat$diff <- dat$today - dat$born
dat$diff_born <- dat$today - dat$born
dat$diff_20 <- dat$today - dat$twenty
dat$diff_before20 <- dat$today - dat$before20
dat$diff_before40 <- dat$today - dat$before40

##consistency check
dat$diff_cat <- NULL
dat$diff_cat[dat$diff_before40 == 0] <- "equal"
dat$diff_cat[dat$diff_before40 > 0] <- "more"
dat$diff_cat[dat$diff_before40 < 0] <- "less"
names(dat)
dat$cat <- paste(dat$check_60, dat$check_65, dat$check_70, dat$check_75, 
                 dat$check_80, dat$check_85, dat$check_90, dat$check_95, dat$check_100,
                 dat$check_105, sep = "")
dat$cat <- as.factor(dat$cat)
levels(dat$cat)

dat$cat2 <- mapvalues(dat$cat, from = c("People are equally kind, honest, nice, and good today compared to about 100 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 105 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 60 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 65 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 70 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 75 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 80 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 85 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 90 years ago",
                                        "People are equally kind, honest, nice, and good today compared to about 95 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 100 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 105 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 60 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 65 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 70 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 75 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 80 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 85 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 90 years ago",
                                        "People are LESS kind, honest, nice, and good today compared to about 95 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 100 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 105 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 60 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 65 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 70 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 75 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 80 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 85 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 90 years ago",
                                        "People are MORE kind, honest, nice, and good today compared to about 95 years ago"),
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

good_melt <- reshape(dat, varying = c("today","twenty","born", "before20","before40"),
                     v.names = "rating", timevar = "time",
                     times = c("today","twenty","born", "before20","before40"),
                     direction = "long",
                     idvar = "participant")
good_melt$time <- as.factor(good_melt$time)
levels(good_melt$time)
good_melt$time <- factor(good_melt$time,levels(good_melt$time)[c(2, 1, 3, 5, 4)])


#####model#####
good_melt$time <- as.factor(good_melt$time)
levels(good_melt$time)
good_mod <- lmer(rating ~ time + (1|participant), data = good_melt)
summary(good_mod)

means <- emmeans(good_mod, specs = ~ time)
means
eff_size(means, sigma = sigma(good_mod), edf = 914)

##set up contrasts
before40 <- c(1,0,0,0,0)
before20 <- c(0,1,0,0,0)
born <- c(0,0,1,0,0)
twenty <- c(0,0,0,1,0)
today <- c(0,0,0,0,1)
contr <- contrast(means, method = list("before40 - before20" = before40 - before20,
                                       "before20 - born" = before20 - born,
                                       "born - twenty" = born - twenty,
                                       "twenty - today" = twenty - today),
                  adjust = "holm")
contr
confint(contr)

####equivalence####
require(parameters)
mod1 <- lmer(rating ~ time + (1|participant), data = good_melt[good_melt$time == "before40" | good_melt$time == "before20",])
mod2 <- lmer(rating ~ time + (1|participant), data = good_melt[good_melt$time == "before20" | good_melt$time == "born",])

summary(good_mod)
parameters::equivalence_test(mod1)
parameters::equivalence_test(mod2)

#####covariates####
dat$diff <- dat$today - dat$born

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

##do liberals also perceive decilne?
t.test(dat$diff[dat$political_ideology_n < 0])
cohensD(dat$diff[dat$political_ideology_n < 0])

#####decline per year#####
dat$dpy_20_today <- (dat$today - dat$twenty)/(dat$age - 20)
dat$dpy_20_born <- (dat$twenty - dat$born)
dat$dpy_total <- (dat$today - dat$born)/(dat$age)

dpy_melt <- reshape(dat, varying = c("dpy_20_today","dpy_20_born", "dpy_total"),
                    v.names = "dpy", timevar = "type", times = c("dpy_20_today","dpy_20_born","dpy_total"),
                    direction = "long", idvar = "participant")
dpy_melt$type <- as.factor(dpy_melt$type)
levels(dpy_melt$type)

####decline per year model###
dpy_mod <- lm(dpy ~ age, data = dpy_melt[dpy_melt$type == "dpy_total",])
summary(dpy_mod)
confint(dpy_mod)

####plot####
min <- c(4.92, 5.01, 5.00, 4.82, 4.14)
max <- c(5.18, 5.27, 5.26, 5.09, 4.41)
bars <- data.frame(min, max)
bars$x <- c(1,2,3,4,5)
points <- c(5.05,5.14,5.13,4.96,4.27)
points <- data.frame(points)
points$x <- c(1,2,3,4,5)
points$y <- c(5.05,5.14,5.13,4.96,4.27)

main_plot <- ggplot(good_melt, aes(x = time, y = rating)) +
  geom_violin() +
  geom_point(position = position_jitter(height = .2, width = .2), color = "gray", alpha = .2) +
  geom_point(data = points, aes(x = x, y = y, size = 3), inherit.aes = FALSE) +
  #stat_summary(fun.y = "mean", size = 1) +
  geom_errorbar(data=bars,aes(x=x,ymin=min,ymax=max),inherit.aes=FALSE, size = 2) +
  coord_cartesian(ylim = c(1, 7)) +
  ylab("Perceived Morality of People in Each Year") +
  xlab(NULL) +
  theme_apa() +
  scale_x_discrete(labels = c("40 Years Before\nParticipant Was Born","20 Years Before\nParticipant Was Born","Year Participant Was Born",
                              "20 Years After\nParticipant Was Born","Current Year")) +
  theme(legend.position = "none")

sessionInfo()


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

all_plot_black <- ggplot(good_melt, aes(x = time, y = rating)) +
  geom_point(position = position_jitter(height = .2, width = .2), color = "gray", alpha = .3) +
  stat_summary(fun.data = "mean_cl_boot", size = 3, geom = "errorbar", color = "white") +
  stat_summary(fun = "mean", size = 2, color = "white") +
  ylab("Perceived Morality") +
  scale_x_discrete(labels = c("40y Before Birth","20y Before Birth","Year of Birth",
                              "20y After Birth","Today")) +
  xlab(label = NULL) +
  theme_black(base_size = 26) +
  coord_cartesian(ylim = c(1,7))
