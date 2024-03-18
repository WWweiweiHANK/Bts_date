##R code for reproducing Figure 2 of "The Illusion of Moral Decline" (Mastroianni, Gilbert, and Wilson 2021)

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
require(lmer)
require(lmerTest)
require(psych)
require(emmeans)
require(blme)

####STUDY 2A####
a <- read.csv("study2a.csv")
dim(a)
head(a)
####
#####clean data, remove any incompletes#####
a$inclusion <- NULL
a$reason <- NULL

a$inclusion[a$Finished == "FALSE"] <- "exclude"
a$reason[a$Finished == "FALSE"] <- "incomplete"
table(a$inclusion)
a <- a[is.na(a$inclusion),]

#####participant demographics#####
dim(a)
table(a$gender)
summary(a$age)
round(prop.table(table(a$race)),2)

#####exclusions#####
a$inclusion[a$check != "Other"] <- "exclude"
a$reason[a$check != "Other"] <- "check"
table(a$inclusion)
levels(as.factor(a$check_4_TEXT))
a$inclusion[a$check_4_TEXT == ""] <- "exclude"
table(a$inclusion)
a$inclusion[a$check_4_TEXT == "Ambivert "] <- "exclude"
table(a$inclusion)
a <- a[is.na(a$inclusion),]
dim(a)

##consistency check
table(a$manip_check)
a$trend <- a$today - a$X2000
summary(a$trend)
describeBy(a$trend, a$manip_check)
a$trend_cat <- NULL
a$trend_cat[a$trend == 0] <- "equal"
a$trend_cat[a$trend > 0] <- "more"
a$trend_cat[a$trend < 0] <- "less"

a$manip_check <- as.factor(a$manip_check)
levels(a$manip_check)
a$strict <- NULL
a$strict[a$trend < 0 & a$manip_check !=  "I said that people today are LESS kind, honest, nice, and good today than people were 20 years ago"] <- "exclude"
a$strict[a$trend == 0 & a$manip_check != "I said that people today are EQUALLY kind, honest, nice, and good today as people were 20 years ago"] <- "exclude"
a$strict[a$trend > 0 & a$manip_check != "I said that people today are MORE kind, honest, nice, and good today than people were 20 years ago"] <- "exclude"
table(a$strict)

#####exclude quality check exclusions, re-report demos#####
a <- a[is.na(a$strict),]
dim(a)
table(a$gender)
summary(a$age)
round(prop.table(table(a$race)),2)

##reshape aa
a_melt <- reshape(a, varying = c("today","X2010","X2000"),
                     v.names = "rating", timevar = "year", times = c("2020","2010","2000"),
                     direction = "long", idvar = "participant")

a_melt <- good_melt[is.na(good_melt$strict),]

####STUDY 2B####
b <- read.csv("study2b.csv")
head(b)
dim(b)


#####data cleaning, remove people who didn't finish#####
b$inclusion <- NULL
b$reason <- NULL

b$inclusion[b$Finished == "FALSE"] <- "exclude"
b$reason[b$Finished == "FALSE"] <- "incomplete"

b <- b[is.na(b$inclusion),]
dim(b)

b$zip <- as.factor(b$zip)

b$inclusion[b$kindergarten != "Kindergarten"] <- "exclude"
b$inclusion[b$zip != "2138"] <- "exclude"
b$inclusion[b$turkey != "Eating turkey"] <- "exclude"
b$reason[b$inclusion == "exclude"] <- "english"
table(b$inclusion)

b <- b[is.na(b$inclusion),]
dim(b)

#####report initial demographics######
table(b$gender)
summary(b$age)
round(prop.table(table(b$race)),2)

#####exclusions######
b$inclusion[b$check != "Other"] <- "exclude"
b$reason[b$check != "Other"] <- "check"
levels(as.factor(b$check_4_TEXT))
b$inclusion[b$check_4_TEXT == ""] <- "exclude"
b$inclusion[b$check_4_TEXT == "word"] <- "exclude"
table(b$inclusion)
b <- b[is.na(b$inclusion),]

dim(b)

#####re-report demographics#####
dim(b)
table(b$gender)
summary(b$age)
round(prop.table(table(b$race)),2)

#####ba cleaning#####
b$today <- mapvalues(b$today, from = c("7 (very)","1 (not very)"), to = c("7","1"))
b$X2018 <- mapvalues(b$X2018, from = c("7 (very)","1 (not very)"), to = c("7","1"))
b$X2016 <- mapvalues(b$X2016, from = c("7 (very)","1 (not very)"), to = c("7","1"))
b$X2014 <- mapvalues(b$X2014, from = c("7 (very)","1 (not very)"), to = c("7","1"))
b$X2012 <- mapvalues(b$X2012, from = c("7 (very)","1 (not very)"), to = c("7","1"))
b$X2010 <- mapvalues(b$X2010, from = c("7 (very)","1 (not very)"), to = c("7","1"))

b$today <- as.integer(as.character(b$today))
b$X2018 <- as.integer(as.character(b$X2018))
b$X2016 <- as.integer(as.character(b$X2016))
b$X2014 <- as.integer(as.character(b$X2014))
b$X2012 <- as.integer(as.character(b$X2012))
b$X2010 <- as.integer(as.character(b$X2010))

#####quality check#####
table(b$manip_check)
b$ten_trend <- b$today - b$X2010
summary(b$ten_trend)
describeBy(b$ten_trend, b$manip_check)
b$trend <- NULL
b$trend[b$ten_trend == 0] <- "equal"
b$trend[b$ten_trend > 0] <- "more"
b$trend[b$ten_trend < 0] <- "less"

table(b$manip_check, b$trend)

b$strict <- NULL
b$strict[b$ten_trend < 0 & b$manip_check !=  "Less kind, honest, nice, good today compared to ten years ago"] <- "exclude"
b$strict[b$ten_trend == 0 & b$manip_check != "Equally kind, honest, nice, good today and ten years ago"] <- "exclude"
b$strict[b$ten_trend > 0 & b$manip_check != "More kind, honest, nice, good today compared to ten years ago"] <- "exclude"

table(b$strict)
b <- b[is.na(b$strict),]

#####reshape#####
b$diff <- b$today - b$X2010
b_melt <- reshape(b, varying = c("today","X2018","X2016","X2014","X2012","X2010"),
                     v.names = "rating", timevar = "year", times = c("2020","2018","2016","2014","2012","2010"),
                     direction = "long", idvar = "participant")


####STUDY 2C####