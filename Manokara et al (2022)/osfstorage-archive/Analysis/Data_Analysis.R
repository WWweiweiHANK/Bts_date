################ Do People Agree on How Positive Emotions are Expressed? A Survey of Four Emotions and Five Modalities across 11 Cultures
#                                        Data Analysis




library (haven)
library (xlsx)


                         ############### Study 1a: USA #################

                         
                         
## Load data for Study 1a                        
                         
USdata <- read_spss ("USA_Study_1a_Wide.sav")                           
        
            

##### US Demographics #####

    # gender

#percent male
sum(USdata$Gender == 1) / nrow (USdata)


    # age

#mean, standard deviation, distribution
mean(USdata$Age)
sd(USdata$Age)

#range
min(USdata$Age)
max(USdata$Age)



    # ethnicity

# Caucasian / White / European = majority ethnicity
sum(!is.na(USdata$Ethnicity)) / nrow(USdata)


                       

##### Descriptives: Frequencies of reported modalities per emotion in the US #####

    # Calculating percentages of overall sample that selected a certain modality for a specific emotion

#feeling moved
movedV <- round(sum(!is.na(USdata$MovedVoice)  &  USdata$MovedVoice == 1) / nrow(USdata) *100, 2)
movedF <- round(sum(!is.na(USdata$MovedFace)  &  USdata$MovedFace == 1) / nrow(USdata) *100, 2)
movedB <- round(sum(!is.na(USdata$MovedBody)  &  USdata$MovedBody == 1) / nrow(USdata) *100, 2)
movedW <- round(sum(!is.na(USdata$MovedWords)  &  USdata$MovedWords == 1) / nrow(USdata) *100, 2)
movedT <- round(sum(!is.na(USdata$MovedTouch)  &  USdata$MovedTouch == 1) / nrow(USdata) *100, 2)
movedO <- round(sum(!is.na(USdata$MovedOther)  &  USdata$MovedOther == 1) / nrow(USdata) *100, 2)

moved_percent <- c(movedV, movedF, movedB, movedW, movedT, movedO)


#gratitude
gratV <- round(sum(!is.na(USdata$GratitudeVoice)  &  USdata$GratitudeVoice == 1) / nrow(USdata) *100, 2)
gratF <- round(sum(!is.na(USdata$GratitudeFace)  &  USdata$GratitudeFace == 1) / nrow(USdata) *100, 2)
gratB <- round(sum(!is.na(USdata$GratitudeBody)  &  USdata$GratitudeBody == 1) / nrow(USdata) *100, 2)
gratW <- round(sum(!is.na(USdata$GratitudeWords)  &  USdata$GratitudeWords == 1) / nrow(USdata) *100, 2)
gratT <- round(sum(!is.na(USdata$GratitudeTouch)  &  USdata$GratitudeTouch == 1) / nrow(USdata) *100, 2)
gratO <- round(sum(!is.na(USdata$GratitudeOther)  &  USdata$GratitudeOther == 1) / nrow(USdata) *100, 2)

grat_percent <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum(!is.na(USdata$InterestVoice)  &  USdata$InterestVoice == 1) / nrow(USdata) *100, 2)
interF <- round(sum(!is.na(USdata$InterestFace)  &  USdata$InterestFace == 1) / nrow(USdata) *100, 2)
interB <- round(sum(!is.na(USdata$InterestBody)  &  USdata$InterestBody == 1) / nrow(USdata) *100, 2)
interW <- round(sum(!is.na(USdata$InterestWords)  &  USdata$InterestWords == 1) / nrow(USdata) *100, 2)
interT <- round(sum(!is.na(USdata$InterestTouch)  &  USdata$InterestTouch == 1) / nrow(USdata) *100, 2)
interO <- round(sum(!is.na(USdata$InterestOther)  &  USdata$InterestOther == 1) / nrow(USdata) *100, 2)

inter_percent <- c(interV, interF, interB, interW, interT, interO)



#triumph
triV <- round(sum(!is.na(USdata$TriumphVoice)  &  USdata$TriumphVoice == 1) / nrow(USdata) *100, 2)
triF <- round(sum(!is.na(USdata$TriumphFace)  &  USdata$TriumphFace == 1) / nrow(USdata) *100, 2)
triB <- round(sum(!is.na(USdata$TriumphBody)  &  USdata$TriumphBody == 1) / nrow(USdata) *100, 2)
triW <- round(sum(!is.na(USdata$TriumphWords)  &  USdata$TriumphWords == 1) / nrow(USdata) *100, 2)
triT <- round(sum(!is.na(USdata$TriumphTouch)  &  USdata$TriumphTouch == 1) / nrow(USdata) *100, 2)
triO <- round(sum(!is.na(USdata$TriumphOther)  &  USdata$TriumphOther == 1) / nrow(USdata) *100, 2)

trium_percent <- c(triV, triF, triB, triW, triT, triO)





##### Constructing 95% confidence intervals
# Procedure: Calculate the proportion (p) that has the characteristic. Then multiply p*(1-p)/n, and obtain the square root. This is the standard error, multiplied by 1.96 for the 95% CI. 

#feeling moved
moved_proportions <- moved_percent / 100
moved_CI <- sqrt(moved_proportions * (1 - moved_proportions) / nrow(USdata)) * 1.96

#gratitude
grat_proportions <- grat_percent / 100
grat_CI <- sqrt(grat_proportions * (1 - grat_proportions) / nrow(USdata)) * 1.96

#interest
inter_proportions <- inter_percent / 100
inter_CI <- sqrt(inter_proportions * (1 - inter_proportions) / nrow(USdata)) * 1.96

#triumph
trium_proportions <- trium_percent / 100
trium_CI <- sqrt(trium_proportions * (1 - trium_proportions) / nrow(USdata)) * 1.96



#to see the CIs
round (moved_percent - moved_CI * 100, 2)
round (moved_percent + moved_CI * 100, 2)
round (grat_percent - grat_CI * 100, 2)
round (grat_percent + grat_CI * 100, 2)
round (inter_percent - inter_CI * 100, 2)
round (inter_percent + inter_CI * 100, 2)
round (trium_percent - trium_CI * 100, 2)
round (trium_percent + trium_CI * 100, 2)




##### Load USA Study 1a long-format data and convert to factors
USdataLong <- read_spss("USA_Study_1a_Long.sav")  

USdataLong$Modalities <- factor(USdataLong$Modalities, levels = unique(USdataLong$Modalities))



######## Cochran's Q test (with post-hocs and effect size calculations)

library("coin")
library("RVAideMemoire")
library(rcompanion)



# Checking whether sample size is large enough (assumption of Cochran's Q) - Procedure: the number of participants with all 1s and 0s cannot be too high, i.e., there must be at least n*k >= 24. k is 6 (number of modalities), so there need to be at least 4 participants who do not have all 1s (no one in the data set has all 0s).

nrow(subset(USdata, USdata$MovedFace == 1 & USdata$MovedVoice == 1 & USdata$MovedBody == 1  & USdata$MovedTouch == 1 & USdata$MovedOther == 1 & USdata$MovedWords == 1))
nrow(subset(USdata, USdata$GratitudeFace == 1 & USdata$GratitudeVoice == 1 & USdata$GratitudeBody == 1 & USdata$GratitudeTouch == 1 & USdata$GratitudeOther == 1 & USdata$GratitudeWords == 1))
nrow(subset(USdata, USdata$InterestFace == 1 & USdata$InterestVoice == 1 & USdata$InterestBody == 1 & USdata$InterestTouch == 1 & USdata$InterestOther == 1 & USdata$InterestWords == 1))
nrow(subset(USdata, USdata$TriumphFace == 1 & USdata$TriumphVoice == 1 & USdata$TriumphBody == 1 & USdata$TriumphTouch == 1 & USdata$TriumphOther == 1 & USdata$TriumphWords == 1))




# function to check whether expected frequencies are below 5 in any cell
check_ft <- function (mod_Matrix) {
  for (i in 1:5) {
    for (j in (i+1):6) {
      if (i != j) {
        print(table (mod_Matrix[,i], mod_Matrix[,j]))
      }
    }
  } 
}




#function to calculate chance corrected effect size (R)
cQ_effectsize <- function (ModMatrix) {
  b <- dim (ModMatrix)[1]
  k <- dim (ModMatrix)[2]
  res <- 0
  
  for (i in 1:k) {
    for (j in 1:(b-1)) {
      for (l in (j+1):b) {
        res <- res + abs(ModMatrix[j,i] - ModMatrix[l,i])
      }
      
    }
    
  }
  
  delta <- 1 /(k * choose(b,2))*res
  pi <- rowSums(ModMatrix)/k
  spi <- 0
  spi2 <- 0
  
  for (i in 1:b) {
    spi <- spi+pi[i]
    spi2 <- spi2 + pi[i]*(1-pi[i])
  }
  
  mudelta <- 2/(b*(b-1))*(spi*(b-spi)-spi2)
  r <- 1-delta/mudelta
  return (round(r,3))
}





#feeling moved
cochran.qtest(Moved ~ Modalities | pp,
              data = USdataLong)

#effect size calculations
Moved_matrix <- cbind(USdata$MovedVoice, USdata$MovedFace, USdata$MovedBody, USdata$MovedWords, USdata$MovedTouch, USdata$MovedOther)
cQ_effectsize(Moved_matrix)
 
#getting the uncorrected effect size
w_Moved <- 1449.43 / (1015 * (6 - 1))
 
check_ft(Moved_matrix)

#post-hocs

CQ_Moved <- pairwiseMcnemar(Moved ~ Modalities | pp,
                     data   = USdataLong,
                     test   = "mcnemar",
                     method = "bonferroni",
                     digits = 7)




#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = USdataLong)

Grat_matrix <- cbind(USdata$GratitudeVoice, USdata$GratitudeFace, USdata$GratitudeBody, USdata$GratitudeWords, USdata$GratitudeTouch, USdata$GratitudeOther)
cQ_effectsize(Grat_matrix)

#getting the uncorrected effect size
w_Grat <- 1408.15 / (1015 * (6 - 1))

check_ft(Grat_matrix)

CQ_Grat <- pairwiseMcnemar(Gratitude ~ Modalities | pp,
                      data   = USdataLong,
                      test   = "mcnemar",
                      method = "bonferroni",
                      digits = 7)




#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = USdataLong)

Inter_matrix <- cbind(USdata$InterestVoice, USdata$InterestFace, USdata$InterestBody, USdata$InterestWords, USdata$InterestTouch, USdata$InterestOther)
cQ_effectsize(Inter_matrix)

#getting the uncorrected effect size
w_Inter <- 1520.52 / (1015 * (6 - 1))

check_ft(Inter_matrix)

CQ_Inter <- pairwiseMcnemar(Interest ~ Modalities | pp,
                           data   = USdataLong,
                           test   = "mcnemar",
                           method = "bonferroni",
                           digits = 7)       




                         
#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = USdataLong)

Trium_matrix <- cbind(USdata$TriumphVoice, USdata$TriumphFace, USdata$TriumphBody, USdata$TriumphWords, USdata$TriumphTouch, USdata$TriumphOther)
cQ_effectsize(Trium_matrix)

#getting the uncorrected effect size
w_Trium <- 1848.93 / (1015 * (6 - 1))

check_ft(Trium_matrix)

CQ_Trium <- pairwiseMcnemar(Triumph ~ Modalities | pp,
                            data   = USdataLong,
                            test   = "mcnemar",
                            method = "bonferroni",
                            digits = 7)                            
                        










        ################ Study 1b: USA sample without definitions and Comparison of the two USA samples ####################



## load data from second US sample                        

US_1b_Data <- read_spss ("USA_Study_1b_Wide.sav")                      


##### USA Study 1b Demographics #####

# gender
#percent male
sum(US_1b_Data$Gender == 1) / nrow (US_1b_Data)


# age
#mean, standard deviation, range
mean(US_1b_Data$Age)
sd(US_1b_Data$Age)
min(US_1b_Data$Age)
max(US_1b_Data$Age)


# majority ethnicity
# Caucasian / White / European
sum(US_1b_Data$Ethnicity, na.rm = T) / nrow(US_1b_Data)





      #### Descriptives for Study 1b sample


    # Frequencies per modality = number of participants selecting that modality to express a given emotion

#feeling moved                          
sum (US_1b_Data$MovedVoice == 1)
sum (US_1b_Data$MovedFace == 1)
sum (US_1b_Data$MovedBody == 1)
sum (US_1b_Data$MovedWords == 1)
sum (US_1b_Data$MovedTouch == 1)
sum (US_1b_Data$MovedOther == 1)

#gratitude
sum (US_1b_Data$GratitudeVoice == 1)
sum (US_1b_Data$GratitudeFace == 1)
sum (US_1b_Data$GratitudeBody == 1)
sum (US_1b_Data$GratitudeWords == 1)
sum (US_1b_Data$GratitudeTouch == 1)
sum (US_1b_Data$GratitudeOther == 1)

#interest
sum (US_1b_Data$InterestVoice == 1)
sum (US_1b_Data$InterestFace == 1)
sum (US_1b_Data$InterestBody == 1)
sum (US_1b_Data$InterestWords == 1)
sum (US_1b_Data$InterestTouch == 1)
sum (US_1b_Data$InterestOther == 1)

#triumph
sum (US_1b_Data$TriumphVoice == 1)
sum (US_1b_Data$TriumphFace == 1)
sum (US_1b_Data$TriumphBody == 1)
sum (US_1b_Data$TriumphWords == 1)
sum (US_1b_Data$TriumphTouch == 1)
sum (US_1b_Data$TriumphOther == 1)


    # Percentages per modality = percent of overall sample that selected this modality for a given emotion

#feeling moved                          
sum (US_1b_Data$MovedVoice == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$MovedFace == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$MovedBody == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$MovedWords == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$MovedTouch == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$MovedOther == 1) / nrow(US_1b_Data)  * 100

#gratitude
sum (US_1b_Data$GratitudeVoice == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$GratitudeFace == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$GratitudeBody == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$GratitudeWords == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$GratitudeTouch == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$GratitudeOther == 1) / nrow(US_1b_Data)  * 100

#interest
sum (US_1b_Data$InterestVoice == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$InterestFace == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$InterestBody == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$InterestWords == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$InterestTouch == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$InterestOther == 1) / nrow(US_1b_Data)  * 100

#triumph
sum (US_1b_Data$TriumphVoice == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$TriumphFace == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$TriumphBody == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$TriumphWords == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$TriumphTouch == 1) / nrow(US_1b_Data)  * 100
sum (US_1b_Data$TriumphOther == 1) / nrow(US_1b_Data)  * 100




    # Constructing 95% confidence intervals for these percentages

# Procedure - put the percentages in a vector per emotion, then construct confidence intervals.

# feeling moved
movedV <- round(sum (US_1b_Data$MovedVoice == 1) / nrow(US_1b_Data)  * 100, 2)
movedF <- round(sum (US_1b_Data$MovedFace == 1) / nrow(US_1b_Data)  * 100, 2)
movedB <- round(sum (US_1b_Data$MovedBody == 1) / nrow(US_1b_Data)  * 100, 2)
movedW <- round(sum (US_1b_Data$MovedWords == 1) / nrow(US_1b_Data)  * 100, 2)
movedT <- round(sum (US_1b_Data$MovedTouch == 1) / nrow(US_1b_Data)  * 100, 2)
movedO <- round(sum (US_1b_Data$MovedOther == 1) / nrow(US_1b_Data)  * 100, 2)

moved_percent_1b <- c(movedV, movedF, movedB, movedW, movedT, movedO)


#gratitude
gratV <- round(sum (US_1b_Data$GratitudeVoice == 1) / nrow(US_1b_Data)  * 100, 2)
gratF <- round(sum (US_1b_Data$GratitudeFace == 1) / nrow(US_1b_Data)  * 100, 2)
gratB <- round(sum (US_1b_Data$GratitudeBody == 1) / nrow(US_1b_Data)  * 100, 2)
gratW <- round(sum (US_1b_Data$GratitudeWords == 1) / nrow(US_1b_Data)  * 100, 2)
gratT <- round(sum (US_1b_Data$GratitudeTouch == 1) / nrow(US_1b_Data)  * 100, 2)
gratO <- round(sum (US_1b_Data$GratitudeOther == 1) / nrow(US_1b_Data)  * 100, 2)

grat_percent_1b <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum (US_1b_Data$InterestVoice == 1) / nrow(US_1b_Data)  * 100, 2)
interF <- round(sum (US_1b_Data$InterestFace == 1) / nrow(US_1b_Data)  * 100, 2)
interB <- round(sum (US_1b_Data$InterestBody == 1) / nrow(US_1b_Data)  * 100, 2)
interW <- round(sum (US_1b_Data$InterestWords == 1) / nrow(US_1b_Data)  * 100, 2)
interT <- round(sum (US_1b_Data$InterestTouch == 1) / nrow(US_1b_Data)  * 100, 2)
interO <- round(sum (US_1b_Data$InterestOther == 1) / nrow(US_1b_Data)  * 100, 2)

inter_percent_1b <- c(interV, interF, interB, interW, interT, interO)


#triumph
triV <- round(sum (US_1b_Data$TriumphVoice == 1) / nrow(US_1b_Data)  * 100, 2)
triF <- round(sum (US_1b_Data$TriumphFace == 1) / nrow(US_1b_Data)  * 100, 2)
triB <- round(sum (US_1b_Data$TriumphBody == 1) / nrow(US_1b_Data)  * 100, 2)
triW <- round(sum (US_1b_Data$TriumphWords == 1) / nrow(US_1b_Data)  * 100, 2)
triT <- round(sum (US_1b_Data$TriumphTouch == 1) / nrow(US_1b_Data)  * 100, 2)
triO <- round(sum (US_1b_Data$TriumphOther == 1) / nrow(US_1b_Data)  * 100, 2)

trium_percent_1b <- c(triV, triF, triB, triW, triT, triO)



# calculations for 95% confidence intervals

#feeling moved
moved_proportions_1b <- moved_percent_1b / 100
moved_CI_1b <- sqrt(moved_proportions_1b * (1 - moved_proportions_1b) / nrow(US_1b_Data)) * 1.96

#gratitude
grat_proportions_1b <- grat_percent_1b / 100
grat_CI_1b <- sqrt(grat_proportions_1b * (1 - grat_proportions_1b) / nrow(US_1b_Data)) * 1.96

#interest
inter_proportions_1b <- inter_percent_1b / 100
inter_CI_1b <- sqrt(inter_proportions_1b * (1 - inter_proportions_1b) / nrow(US_1b_Data)) * 1.96

#triumph
trium_proportions_1b <- trium_percent_1b / 100
trium_CI_1b <- sqrt(trium_proportions_1b * (1 - trium_proportions_1b) / nrow(US_1b_Data)) * 1.96



#to see the CIs
round (moved_percent_1b - moved_CI_1b * 100, 2)
round (moved_percent_1b + moved_CI_1b * 100, 2)
round (grat_percent_1b - grat_CI_1b * 100, 2)
round (grat_percent_1b + grat_CI_1b * 100, 2)
round (inter_percent_1b - inter_CI_1b * 100, 2)
round (inter_percent_1b + inter_CI_1b * 100, 2)
round (trium_percent_1b - trium_CI_1b * 100, 2)
round (trium_percent_1b + trium_CI_1b * 100, 2)








      ##### Analysis within Study 1b USA sample #####


######## Load long format Study 1b data from here ########
USdataLong_1b <- read_spss("USA_Study_1b_Long.sav")

USdataLong_1b$Modalities <- factor(USdataLong_1b$Modalities, levels = unique(USdataLong_1b$Modalities))



# Checking whether sample size is large enough - Procedure: the number of participants with all 1s and 0s cannot be too high, i.e., there must be at least n*k >= 24. k is 6 (number of modalities), so there need to be at least 4 participants who do not have all 1s (no one in the data set has all 0s).

nrow(subset(US_1b_Data, US_1b_Data$MovedFace == 1 & US_1b_Data$MovedVoice == 1 & US_1b_Data$MovedBody == 1  & US_1b_Data$MovedTouch == 1 & US_1b_Data$MovedOther == 1 & US_1b_Data$MovedWords == 1))
nrow(subset(US_1b_Data, US_1b_Data$GratitudeFace == 1 & US_1b_Data$GratitudeVoice == 1 & US_1b_Data$GratitudeBody == 1 & US_1b_Data$GratitudeTouch == 1 & US_1b_Data$GratitudeOther == 1 & US_1b_Data$GratitudeWords == 1))
nrow(subset(US_1b_Data, US_1b_Data$InterestFace == 1 & US_1b_Data$InterestVoice == 1 & US_1b_Data$InterestBody == 1 & US_1b_Data$InterestTouch == 1 & US_1b_Data$InterestOther == 1 & US_1b_Data$InterestWords == 1))
nrow(subset(US_1b_Data, US_1b_Data$TriumphFace == 1 & US_1b_Data$TriumphVoice == 1 & US_1b_Data$TriumphBody == 1 & US_1b_Data$TriumphTouch == 1 & US_1b_Data$TriumphOther == 1 & US_1b_Data$TriumphWords == 1))



      #### Cochran's Q test within Study 1b sample


#feeling moved
cochran.qtest(Moved ~ Modalities | pp,
              data = USdataLong_1b)

#effect size calculations
Moved_matrix <- cbind(US_1b_Data$MovedVoice, US_1b_Data$MovedFace, US_1b_Data$MovedBody, US_1b_Data$MovedWords, US_1b_Data$MovedTouch, US_1b_Data$MovedOther)
cQ_effectsize(Moved_matrix)

check_ft(Moved_matrix)

#post-hocs
CQ_Moved <- pairwiseMcnemar(Moved ~ Modalities | pp,
                            data   = USdataLong_1b,
                            test   = "mcnemar",
                            method = "bonferroni",
                            digits = 7)



#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = USdataLong_1b)

Grat_matrix <- cbind(US_1b_Data$GratitudeVoice, US_1b_Data$GratitudeFace, US_1b_Data$GratitudeBody, US_1b_Data$GratitudeWords, US_1b_Data$GratitudeTouch, US_1b_Data$GratitudeOther)
cQ_effectsize(Grat_matrix)

check_ft(Grat_matrix)

CQ_Grat <- pairwiseMcnemar(Gratitude ~ Modalities | pp,
                           data   = USdataLong_1b,
                           test   = "mcnemar",
                           method = "bonferroni",
                           digits = 7)




#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = USdataLong_1b)


Inter_matrix <- cbind(US_1b_Data$InterestVoice, US_1b_Data$InterestFace, US_1b_Data$InterestBody, US_1b_Data$InterestWords, US_1b_Data$InterestTouch, US_1b_Data$InterestOther)
cQ_effectsize(Inter_matrix)

check_ft(Inter_matrix)

CQ_Inter <- pairwiseMcnemar(Interest ~ Modalities | pp,
                            data   = USdataLong_1b,
                            test   = "mcnemar",
                            method = "bonferroni",
                            digits = 7)       




#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = USdataLong_1b)

Trium_matrix <- cbind(US_1b_Data$TriumphVoice, US_1b_Data$TriumphFace, US_1b_Data$TriumphBody, US_1b_Data$TriumphWords, US_1b_Data$TriumphTouch, US_1b_Data$TriumphOther)
cQ_effectsize(Trium_matrix)

check_ft(Trium_matrix)

CQ_Trium <- pairwiseMcnemar(Triumph ~ Modalities | pp,
                            data   = USdataLong_1b,
                            test   = "mcnemar",
                            method = "bonferroni",
                            digits = 7)                            






#### Comparison of the two USA samples                  

save_data <- read_spss("USA_Study_1a_1b_Comparison.sav")


## Frequentist chi-square approach for comparing the two samples (see JASP file for Bayesian)

p.values <- numeric (length = 20)

p.values[1] <- chisq.test(table (save_data$Sample, save_data$MovedVoice), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$MovedVoice), digits = 6)
p.values[2] <- chisq.test(table (save_data$Sample, save_data$MovedFace), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$MovedFace), digits = 6)
p.values[3] <- chisq.test(table (save_data$Sample, save_data$MovedBody), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$MovedBody), digits = 6)
p.values[4] <- chisq.test(table (save_data$Sample, save_data$MovedWords), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$MovedWords), digits = 6)
p.values[5] <- chisq.test(table (save_data$Sample, save_data$MovedTouch), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$MovedTouch), digits = 6)

p.values[6] <- chisq.test(table (save_data$Sample, save_data$GratitudeVoice), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$GratitudeVoice), digits = 6)
p.values[7] <- chisq.test(table (save_data$Sample, save_data$GratitudeFace), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$GratitudeFace), digits = 6)
p.values[8] <- chisq.test(table (save_data$Sample, save_data$GratitudeBody), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$GratitudeBody), digits = 6)
p.values[9] <- chisq.test(table (save_data$Sample, save_data$GratitudeWords), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$GratitudeWords), digits = 6)
p.values[10] <- chisq.test(table (save_data$Sample, save_data$GratitudeTouch), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$GratitudeTouch), digits = 6)

p.values[11] <- chisq.test(table (save_data$Sample, save_data$InterestVoice), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$InterestVoice), digits = 6)
p.values[12] <- chisq.test(table (save_data$Sample, save_data$InterestFace), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$InterestFace), digits = 6)
p.values[13] <- chisq.test(table (save_data$Sample, save_data$InterestBody), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$InterestBody), digits = 6)
p.values[14] <- chisq.test(table (save_data$Sample, save_data$InterestWords), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$InterestWords), digits = 6)
p.values[15] <- chisq.test(table (save_data$Sample, save_data$InterestTouch), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$InterestTouch), digits = 6)

p.values[16] <- chisq.test(table (save_data$Sample, save_data$TriumphVoice), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$TriumphVoice), digits = 6)
p.values[17] <- chisq.test(table (save_data$Sample, save_data$TriumphFace), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$TriumphFace), digits = 6)
p.values[18] <- chisq.test(table (save_data$Sample, save_data$TriumphBody), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$TriumphBody), digits = 6)
p.values[19] <- chisq.test(table (save_data$Sample, save_data$TriumphWords), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$TriumphWords), digits = 6)
p.values[20] <- chisq.test(table (save_data$Sample, save_data$TriumphTouch), correct = F)$p.value
cramerV(table (save_data$Sample, save_data$TriumphTouch), digits = 6)

p.adjust(p = p.values, method = "bonferroni")














                         ############### Study 2: Cross-cultural #################

                        
## load data (Cross-cultural)                         
                         
CCdata <- read_spss ("Cross_cultural_Study_2_Wide.sav")



                         
##### Demographics #####

    # sample sizes per country

for (i in unique(CCdata$Country)) {    
  print(paste("number of participants in", i, "is", nrow (subset(CCdata, CCdata$Country == i))))
}



    # gender

#percent male

for (i in unique(CCdata$Country)) {
  print(paste("male percentage in", i, "is", sum(subset(CCdata, CCdata$Country == i)$Gender == 1) / nrow (subset(CCdata, CCdata$Country == i))* 100))
  print(paste("female percentage in", i, "is", sum(subset(CCdata, CCdata$Country == i)$Gender == 2) / nrow (subset(CCdata, CCdata$Country == i))* 100))
  print(paste("do not wish to disclose percentage in", i, "is", sum(subset(CCdata, CCdata$Country == i)$Gender == 3) / nrow (subset(CCdata, CCdata$Country == i))* 100))
  print(paste("other percentage in", i, "is", sum(subset(CCdata, CCdata$Country == i)$Gender == 4) / nrow (subset(CCdata, CCdata$Country == i))* 100))
}


# age

#mean, standard deviation, distribution of age

for (i in unique(CCdata$Country)) {
  print(paste("age mean in", i, "is", mean(subset(CCdata, CCdata$Country == i)$Age)))
  print(paste("age SD in", i, "is", sd(subset(CCdata, CCdata$Country == i)$Age)))
  print(paste("age min in", i, "is", min(subset(CCdata, CCdata$Country == i)$Age)))
  print(paste("age max in", i, "is", max(subset(CCdata, CCdata$Country == i)$Age)))
  }



# ethnicity - ethnic majority = 1
for (i in unique(CCdata$Country)) {
  print(paste("percent of ethnic majority in", i, "is", sum(subset(CCdata, CCdata$Country == i)$Ethnicity == 1, na.rm = T) / nrow (subset(CCdata, CCdata$Country == i))* 100))
}

      # NOTE - for finding percentage of ethnic majority in Canada, use this:                    
sum(!is.na(subset(CCdata, CCdata$Country == "Canada")$Ethnicity_1)) / nrow(subset(CCdata, CCdata$Country == "Canada")) * 100           
                         

## Ethnicity percentage can also be calculated using the Ethnic_Majority variable, where "Yes" means ethnic majority, "No" means not ethnic majority, and NA or "" means no information collected (Netherlands).

for (i in unique(CCdata$Country)) {
  print(paste("percent of ethnic majority in", i, "is", sum(subset(CCdata, CCdata$Country == i)$Ethnic_Majority == "Yes", na.rm = T) / nrow (subset(CCdata, CCdata$Country == i))* 100))
}







############# Descriptives of modality choices ################

    ########## Calculating frequencies = number of participants who selected a particular modality for expressing an emotion within each country


for (i in unique(CCdata$Country)) {
  
  print("")
  print(i)
  print("")
  
  
  #feeling moved                          
  print("Feeling moved")
  print("")
  
  print(paste("frequency moved voice in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$MovedVoice == 1)))
  print(paste("frequency moved face in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$MovedFace == 1)))
  print(paste("frequency moved body in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$MovedBody == 1)))
  print(paste("frequency moved words in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$MovedWords == 1)))
  print(paste("frequency moved touch in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$MovedTouch == 1)))
  print(paste("frequency moved other in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$MovedOther == 1)))
  
  
  #gratitude
  print("")
  print("Gratitude")
  print("")
  
  print(paste("frequency gratitude voice in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$GratitudeVoice == 1)))
  print(paste("frequency gratitude face in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$GratitudeFace == 1)))
  print(paste("frequency gratitude body in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$GratitudeBody == 1)))
  print(paste("frequency gratitude words in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$GratitudeWords == 1)))
  print(paste("frequency gratitude touch in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$GratitudeTouch == 1)))
  print(paste("frequency gratitude other in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$GratitudeOther == 1)))
  
  
  
  #interest
  print("")
  print("Interest")
  print("")
  
  print(paste("frequency interest voice in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$InterestVoice == 1)))
  print(paste("frequency interest face in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$InterestFace == 1)))
  print(paste("frequency interest body in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$InterestBody == 1)))
  print(paste("frequency interest words in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$InterestWords == 1)))
  print(paste("frequency interest touch in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$InterestTouch == 1)))
  print(paste("frequency interest other in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$InterestOther == 1)))
  
  
  
  #triumph
  print("")
  print("Triumph")
  print("")
  
  
  print(paste("frequency triumph voice in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$TriumphVoice == 1)))
  print(paste("frequency triumph face in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$TriumphFace == 1)))
  print(paste("frequency triumph body in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$TriumphBody == 1)))
  print(paste("frequency triumph words in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$TriumphWords == 1)))
  print(paste("frequency triumph touch in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$TriumphTouch == 1)))
  print(paste("frequency triumph other in", i, "is", 
              sum (subset(CCdata, CCdata$Country == i)$TriumphOther == 1)))
  
  
  print("")
  print ("-------------------------")
  
  
}









    ######## Calculating percentages of participants who selected a particular modality for expressing an emotion within each country



for (i in unique(CCdata$Country)) {
  
  print("")
  print(i)
  print("")
  
  
  #feeling moved                          
  print("Feeling moved")
  print("")
  
  print(paste("percentage moved voice in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$MovedVoice == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage moved face in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$MovedFace == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage moved body in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$MovedBody == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage moved words in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$MovedWords == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage moved touch in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$MovedTouch == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage moved other in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$MovedOther == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
 
  
  #gratitude
  print("")
  print("Gratitude")
  print("")
  
  print(paste("percentage gratitude voice in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$GratitudeVoice == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage gratitude face in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$GratitudeFace == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage gratitude body in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$GratitudeBody == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage gratitude words in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$GratitudeWords == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage gratitude touch in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$GratitudeTouch == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage gratitude other in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$GratitudeOther == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  
  
  
  #interest
  print("")
  print("Interest")
  print("")
  
  print(paste("percentage interest voice in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$InterestVoice == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage interest face in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$InterestFace == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage interest body in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$InterestBody == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage interest words in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$InterestWords == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage interest touch in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$InterestTouch == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage interest other in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$InterestOther == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  
  
  
  #triumph
  print("")
  print("Triumph")
  print("")
  
  
  print(paste("percentage triumph voice in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$TriumphVoice == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage triumph face in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$TriumphFace == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage triumph body in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$TriumphBody == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage triumph words in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$TriumphWords == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage triumph touch in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$TriumphTouch == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  print(paste("percentage triumph other in", i, "is", 
              round(sum (subset(CCdata, CCdata$Country == i)$TriumphOther == 1) / nrow(subset(CCdata, CCdata$Country == i)) *100, 2)))
  
  
  print("")
  print ("-------------------------")
  
  
   }









    ####### Constructing 95% confidence intervals around country modality percentages (proportions)

          # Procedure: For each country, create a vector with the percentages per emotion, then calculate the confidence intervals. 



##Australia

#feeling moved
movedV <- round(sum (subset(CCdata, CCdata$Country == "Australia")$MovedVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
movedF <- round(sum (subset(CCdata, CCdata$Country == "Australia")$MovedFace == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
movedB <- round(sum (subset(CCdata, CCdata$Country == "Australia")$MovedBody == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
movedW <- round(sum (subset(CCdata, CCdata$Country == "Australia")$MovedWords == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
movedT <- round(sum (subset(CCdata, CCdata$Country == "Australia")$MovedTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
movedO <- round(sum (subset(CCdata, CCdata$Country == "Australia")$MovedOther == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)

moved_percent_2 <- c(movedV, movedF, movedB, movedW, movedT, movedO)



#gratitude
gratV <- round(sum (subset(CCdata, CCdata$Country == "Australia")$GratitudeVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
gratF <- round(sum (subset(CCdata, CCdata$Country == "Australia")$GratitudeFace == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
gratB <- round(sum (subset(CCdata, CCdata$Country == "Australia")$GratitudeBody == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
gratW <- round(sum (subset(CCdata, CCdata$Country == "Australia")$GratitudeWords == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
gratT <- round(sum (subset(CCdata, CCdata$Country == "Australia")$GratitudeTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
gratO <- round(sum (subset(CCdata, CCdata$Country == "Australia")$GratitudeOther == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)

grat_percent_2 <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum (subset(CCdata, CCdata$Country == "Australia")$InterestVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
interF <- round(sum (subset(CCdata, CCdata$Country == "Australia")$InterestFace == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
interB <- round(sum (subset(CCdata, CCdata$Country == "Australia")$InterestBody == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
interW <- round(sum (subset(CCdata, CCdata$Country == "Australia")$InterestWords == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
interT <- round(sum (subset(CCdata, CCdata$Country == "Australia")$InterestTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
interO <- round(sum (subset(CCdata, CCdata$Country == "Australia")$InterestOther == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)

inter_percent_2 <- c(interV, interF, interB, interW, interT, interO)



#triumph
triV <- round(sum (subset(CCdata, CCdata$Country == "Australia")$TriumphVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
triF <- round(sum (subset(CCdata, CCdata$Country == "Australia")$TriumphFace == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
triB <- round(sum (subset(CCdata, CCdata$Country == "Australia")$TriumphBody == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
triW <- round(sum (subset(CCdata, CCdata$Country == "Australia")$TriumphWords == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
triT <- round(sum (subset(CCdata, CCdata$Country == "Australia")$TriumphTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)
triO <- round(sum (subset(CCdata, CCdata$Country == "Australia")$TriumphOther == 1) / nrow(subset(CCdata, CCdata$Country == "Australia")) *100, 2)

trium_percent_2 <- c(triV, triF, triB, triW, triT, triO)



# calculating 95% confidence intervals


#feeling moved
moved_proportions_2 <- moved_percent_2 / 100
moved_CI_2 <- sqrt(moved_proportions_2 * (1 - moved_proportions_2) / nrow(subset(CCdata, CCdata$Country == "Australia"))) * 1.96

#gratitude
grat_proportions_2 <- grat_percent_2 / 100
grat_CI_2 <- sqrt(grat_proportions_2 * (1 - grat_proportions_2) / nrow(subset(CCdata, CCdata$Country == "Australia"))) * 1.96

#interest
inter_proportions_2 <- inter_percent_2 / 100
inter_CI_2 <- sqrt(inter_proportions_2 * (1 - inter_proportions_2) / nrow(subset(CCdata, CCdata$Country == "Australia"))) * 1.96

#triumph
trium_proportions_2 <- trium_percent_2 / 100
trium_CI_2 <- sqrt(trium_proportions_2 * (1 - trium_proportions_2) / nrow(subset(CCdata, CCdata$Country == "Australia"))) * 1.96



#to see the CIs
round (moved_percent_2 - moved_CI_2 * 100, 2)
round (moved_percent_2 + moved_CI_2 * 100, 2)
round (grat_percent_2 - grat_CI_2 * 100, 2)
round (grat_percent_2 + grat_CI_2 * 100, 2)
round (inter_percent_2 - inter_CI_2 * 100, 2)
round (inter_percent_2 + inter_CI_2 * 100, 2)
round (trium_percent_2 - trium_CI_2 * 100, 2)
round (trium_percent_2 + trium_CI_2 * 100, 2)







#Austria


#feeling moved
movedV <- round(sum (subset(CCdata, CCdata$Country == "Austria")$MovedVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
movedF <- round(sum (subset(CCdata, CCdata$Country == "Austria")$MovedFace == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
movedB <- round(sum (subset(CCdata, CCdata$Country == "Austria")$MovedBody == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
movedW <- round(sum (subset(CCdata, CCdata$Country == "Austria")$MovedWords == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
movedT <- round(sum (subset(CCdata, CCdata$Country == "Austria")$MovedTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
movedO <- round(sum (subset(CCdata, CCdata$Country == "Austria")$MovedOther == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)

moved_percent_3 <- c(movedV, movedF, movedB, movedW, movedT, movedO)


#gratitude
gratV <- round(sum (subset(CCdata, CCdata$Country == "Austria")$GratitudeVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
gratF <- round(sum (subset(CCdata, CCdata$Country == "Austria")$GratitudeFace == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
gratB <- round(sum (subset(CCdata, CCdata$Country == "Austria")$GratitudeBody == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
gratW <- round(sum (subset(CCdata, CCdata$Country == "Austria")$GratitudeWords == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
gratT <- round(sum (subset(CCdata, CCdata$Country == "Austria")$GratitudeTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
gratO <- round(sum (subset(CCdata, CCdata$Country == "Austria")$GratitudeOther == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)

grat_percent_3 <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum (subset(CCdata, CCdata$Country == "Austria")$InterestVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
interF <- round(sum (subset(CCdata, CCdata$Country == "Austria")$InterestFace == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
interB <- round(sum (subset(CCdata, CCdata$Country == "Austria")$InterestBody == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
interW <- round(sum (subset(CCdata, CCdata$Country == "Austria")$InterestWords == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
interT <- round(sum (subset(CCdata, CCdata$Country == "Austria")$InterestTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
interO <- round(sum (subset(CCdata, CCdata$Country == "Austria")$InterestOther == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)

inter_percent_3 <- c(interV, interF, interB, interW, interT, interO)



#triumph
triV <- round(sum (subset(CCdata, CCdata$Country == "Austria")$TriumphVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
triF <- round(sum (subset(CCdata, CCdata$Country == "Austria")$TriumphFace == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
triB <- round(sum (subset(CCdata, CCdata$Country == "Austria")$TriumphBody == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
triW <- round(sum (subset(CCdata, CCdata$Country == "Austria")$TriumphWords == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
triT <- round(sum (subset(CCdata, CCdata$Country == "Austria")$TriumphTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)
triO <- round(sum (subset(CCdata, CCdata$Country == "Austria")$TriumphOther == 1) / nrow(subset(CCdata, CCdata$Country == "Austria")) *100, 2)

trium_percent_3 <- c(triV, triF, triB, triW, triT, triO)



# calculating 95% confidence intervals


#feeling moved
moved_proportions_3 <- moved_percent_3 / 100
moved_CI_3 <- sqrt(moved_proportions_3 * (1 - moved_proportions_3) / nrow(subset(CCdata, CCdata$Country == "Austria"))) * 1.96

#gratitude
grat_proportions_3 <- grat_percent_3 / 100
grat_CI_3 <- sqrt(grat_proportions_3 * (1 - grat_proportions_3) / nrow(subset(CCdata, CCdata$Country == "Austria"))) * 1.96

#interest
inter_proportions_3 <- inter_percent_3 / 100
inter_CI_3 <- sqrt(inter_proportions_3 * (1 - inter_proportions_3) / nrow(subset(CCdata, CCdata$Country == "Austria"))) * 1.96

#triumph
trium_proportions_3 <- trium_percent_3 / 100
trium_CI_3 <- sqrt(trium_proportions_3 * (1 - trium_proportions_3) / nrow(subset(CCdata, CCdata$Country == "Austria"))) * 1.96



#to see the CIs
round (moved_percent_3 - moved_CI_3 * 100, 2)
round (moved_percent_3 + moved_CI_3 * 100, 2)
round (grat_percent_3 - grat_CI_3 * 100, 2)
round (grat_percent_3 + grat_CI_3 * 100, 2)
round (inter_percent_3 - inter_CI_3 * 100, 2)
round (inter_percent_3 + inter_CI_3 * 100, 2)
round (trium_percent_3 - trium_CI_3 * 100, 2)
round (trium_percent_3 + trium_CI_3 * 100, 2)










#Canada


#feeling moved
movedV <- round(sum (subset(CCdata, CCdata$Country == "Canada")$MovedVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
movedF <- round(sum (subset(CCdata, CCdata$Country == "Canada")$MovedFace == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
movedB <- round(sum (subset(CCdata, CCdata$Country == "Canada")$MovedBody == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
movedW <- round(sum (subset(CCdata, CCdata$Country == "Canada")$MovedWords == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
movedT <- round(sum (subset(CCdata, CCdata$Country == "Canada")$MovedTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
movedO <- round(sum (subset(CCdata, CCdata$Country == "Canada")$MovedOther == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)

moved_percent_4 <- c(movedV, movedF, movedB, movedW, movedT, movedO)


#gratitude
gratV <- round(sum (subset(CCdata, CCdata$Country == "Canada")$GratitudeVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
gratF <- round(sum (subset(CCdata, CCdata$Country == "Canada")$GratitudeFace == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
gratB <- round(sum (subset(CCdata, CCdata$Country == "Canada")$GratitudeBody == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
gratW <- round(sum (subset(CCdata, CCdata$Country == "Canada")$GratitudeWords == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
gratT <- round(sum (subset(CCdata, CCdata$Country == "Canada")$GratitudeTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
gratO <- round(sum (subset(CCdata, CCdata$Country == "Canada")$GratitudeOther == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)

grat_percent_4 <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum (subset(CCdata, CCdata$Country == "Canada")$InterestVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
interF <- round(sum (subset(CCdata, CCdata$Country == "Canada")$InterestFace == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
interB <- round(sum (subset(CCdata, CCdata$Country == "Canada")$InterestBody == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
interW <- round(sum (subset(CCdata, CCdata$Country == "Canada")$InterestWords == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
interT <- round(sum (subset(CCdata, CCdata$Country == "Canada")$InterestTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
interO <- round(sum (subset(CCdata, CCdata$Country == "Canada")$InterestOther == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)

inter_percent_4 <- c(interV, interF, interB, interW, interT, interO)



#triumph
triV <- round(sum (subset(CCdata, CCdata$Country == "Canada")$TriumphVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
triF <- round(sum (subset(CCdata, CCdata$Country == "Canada")$TriumphFace == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
triB <- round(sum (subset(CCdata, CCdata$Country == "Canada")$TriumphBody == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
triW <- round(sum (subset(CCdata, CCdata$Country == "Canada")$TriumphWords == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
triT <- round(sum (subset(CCdata, CCdata$Country == "Canada")$TriumphTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)
triO <- round(sum (subset(CCdata, CCdata$Country == "Canada")$TriumphOther == 1) / nrow(subset(CCdata, CCdata$Country == "Canada")) *100, 2)

trium_percent_4 <- c(triV, triF, triB, triW, triT, triO)



# calculating 95% confidence intervals


#feeling moved
moved_proportions_4 <- moved_percent_4 / 100
moved_CI_4 <- sqrt(moved_proportions_4 * (1 - moved_proportions_4) / nrow(subset(CCdata, CCdata$Country == "Canada"))) * 1.96

#gratitude
grat_proportions_4 <- grat_percent_4 / 100
grat_CI_4 <- sqrt(grat_proportions_4 * (1 - grat_proportions_4) / nrow(subset(CCdata, CCdata$Country == "Canada"))) * 1.96

#interest
inter_proportions_4 <- inter_percent_4 / 100
inter_CI_4 <- sqrt(inter_proportions_4 * (1 - inter_proportions_4) / nrow(subset(CCdata, CCdata$Country == "Canada"))) * 1.96

#triumph
trium_proportions_4 <- trium_percent_4 / 100
trium_CI_4 <- sqrt(trium_proportions_4 * (1 - trium_proportions_4) / nrow(subset(CCdata, CCdata$Country == "Canada"))) * 1.96



#to see the CIs
round (moved_percent_4 - moved_CI_4 * 100, 2)
round (moved_percent_4 + moved_CI_4 * 100, 2)
round (grat_percent_4 - grat_CI_4 * 100, 2)
round (grat_percent_4 + grat_CI_4 * 100, 2)
round (inter_percent_4 - inter_CI_4 * 100, 2)
round (inter_percent_4 + inter_CI_4 * 100, 2)
round (trium_percent_4 - trium_CI_4 * 100, 2)
round (trium_percent_4 + trium_CI_4 * 100, 2)







##China

#feeling moved
movedV <- round(sum (subset(CCdata, CCdata$Country == "China")$MovedVoice == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
movedF <- round(sum (subset(CCdata, CCdata$Country == "China")$MovedFace == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
movedB <- round(sum (subset(CCdata, CCdata$Country == "China")$MovedBody == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
movedW <- round(sum (subset(CCdata, CCdata$Country == "China")$MovedWords == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
movedT <- round(sum (subset(CCdata, CCdata$Country == "China")$MovedTouch == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
movedO <- round(sum (subset(CCdata, CCdata$Country == "China")$MovedOther == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)

moved_percent_5 <- c(movedV, movedF, movedB, movedW, movedT, movedO)


#gratitude
gratV <- round(sum (subset(CCdata, CCdata$Country == "China")$GratitudeVoice == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
gratF <- round(sum (subset(CCdata, CCdata$Country == "China")$GratitudeFace == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
gratB <- round(sum (subset(CCdata, CCdata$Country == "China")$GratitudeBody == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
gratW <- round(sum (subset(CCdata, CCdata$Country == "China")$GratitudeWords == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
gratT <- round(sum (subset(CCdata, CCdata$Country == "China")$GratitudeTouch == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
gratO <- round(sum (subset(CCdata, CCdata$Country == "China")$GratitudeOther == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)

grat_percent_5 <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum (subset(CCdata, CCdata$Country == "China")$InterestVoice == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
interF <- round(sum (subset(CCdata, CCdata$Country == "China")$InterestFace == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
interB <- round(sum (subset(CCdata, CCdata$Country == "China")$InterestBody == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
interW <- round(sum (subset(CCdata, CCdata$Country == "China")$InterestWords == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
interT <- round(sum (subset(CCdata, CCdata$Country == "China")$InterestTouch == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
interO <- round(sum (subset(CCdata, CCdata$Country == "China")$InterestOther == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)

inter_percent_5 <- c(interV, interF, interB, interW, interT, interO)



#triumph
triV <- round(sum (subset(CCdata, CCdata$Country == "China")$TriumphVoice == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
triF <- round(sum (subset(CCdata, CCdata$Country == "China")$TriumphFace == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
triB <- round(sum (subset(CCdata, CCdata$Country == "China")$TriumphBody == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
triW <- round(sum (subset(CCdata, CCdata$Country == "China")$TriumphWords == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
triT <- round(sum (subset(CCdata, CCdata$Country == "China")$TriumphTouch == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)
triO <- round(sum (subset(CCdata, CCdata$Country == "China")$TriumphOther == 1) / nrow(subset(CCdata, CCdata$Country == "China")) *100, 2)

trium_percent_5 <- c(triV, triF, triB, triW, triT, triO)



# calculating 95% confidence intervals


#feeling moved
moved_proportions_5 <- moved_percent_5 / 100
moved_CI_5 <- sqrt(moved_proportions_5 * (1 - moved_proportions_5) / nrow(subset(CCdata, CCdata$Country == "China"))) * 1.96

#gratitude
grat_proportions_5 <- grat_percent_5 / 100
grat_CI_5 <- sqrt(grat_proportions_5 * (1 - grat_proportions_5) / nrow(subset(CCdata, CCdata$Country == "China"))) * 1.96

#interest
inter_proportions_5 <- inter_percent_5 / 100
inter_CI_5 <- sqrt(inter_proportions_5 * (1 - inter_proportions_5) / nrow(subset(CCdata, CCdata$Country == "China"))) * 1.96

#triumph
trium_proportions_5 <- trium_percent_5 / 100
trium_CI_5 <- sqrt(trium_proportions_5 * (1 - trium_proportions_5) / nrow(subset(CCdata, CCdata$Country == "China"))) * 1.96



#to see the CIs
round (moved_percent_5 - moved_CI_5 * 100, 2)
round (moved_percent_5 + moved_CI_5 * 100, 2)
round (grat_percent_5 - grat_CI_5 * 100, 2)
round (grat_percent_5 + grat_CI_5 * 100, 2)
round (inter_percent_5 - inter_CI_5 * 100, 2)
round (inter_percent_5 + inter_CI_5 * 100, 2)
round (trium_percent_5 - trium_CI_5 * 100, 2)
round (trium_percent_5 + trium_CI_5 * 100, 2)








#Croatia

#feeling moved
movedV <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$MovedVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
movedF <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$MovedFace == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
movedB <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$MovedBody == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
movedW <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$MovedWords == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
movedT <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$MovedTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
movedO <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$MovedOther == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)

moved_percent_6 <- c(movedV, movedF, movedB, movedW, movedT, movedO)


#gratitude
gratV <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$GratitudeVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
gratF <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$GratitudeFace == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
gratB <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$GratitudeBody == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
gratW <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$GratitudeWords == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
gratT <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$GratitudeTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
gratO <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$GratitudeOther == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)

grat_percent_6 <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$InterestVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
interF <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$InterestFace == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
interB <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$InterestBody == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
interW <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$InterestWords == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
interT <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$InterestTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
interO <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$InterestOther == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)

inter_percent_6 <- c(interV, interF, interB, interW, interT, interO)



#triumph
triV <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$TriumphVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
triF <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$TriumphFace == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
triB <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$TriumphBody == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
triW <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$TriumphWords == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
triT <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$TriumphTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)
triO <- round(sum (subset(CCdata, CCdata$Country == "Croatia")$TriumphOther == 1) / nrow(subset(CCdata, CCdata$Country == "Croatia")) *100, 2)

trium_percent_6 <- c(triV, triF, triB, triW, triT, triO)



# calculating 95% confidence intervals


#feeling moved
moved_proportions_6 <- moved_percent_6 / 100
moved_CI_6 <- sqrt(moved_proportions_6 * (1 - moved_proportions_6) / nrow(subset(CCdata, CCdata$Country == "Croatia"))) * 1.96

#gratitude
grat_proportions_6 <- grat_percent_6 / 100
grat_CI_6 <- sqrt(grat_proportions_6 * (1 - grat_proportions_6) / nrow(subset(CCdata, CCdata$Country == "Croatia"))) * 1.96

#interest
inter_proportions_6 <- inter_percent_6 / 100
inter_CI_6 <- sqrt(inter_proportions_6 * (1 - inter_proportions_6) / nrow(subset(CCdata, CCdata$Country == "Croatia"))) * 1.96

#triumph
trium_proportions_6 <- trium_percent_6 / 100
trium_CI_6 <- sqrt(trium_proportions_6 * (1 - trium_proportions_6) / nrow(subset(CCdata, CCdata$Country == "Croatia"))) * 1.96



#to see the CIs
round (moved_percent_6 - moved_CI_6 * 100, 2)
round (moved_percent_6 + moved_CI_6 * 100, 2)
round (grat_percent_6 - grat_CI_6 * 100, 2)
round (grat_percent_6 + grat_CI_6 * 100, 2)
round (inter_percent_6 - inter_CI_6 * 100, 2)
round (inter_percent_6 + inter_CI_6 * 100, 2)
round (trium_percent_6 - trium_CI_6 * 100, 2)
round (trium_percent_6 + trium_CI_6 * 100, 2)










#England


#feeling moved
movedV <- round(sum (subset(CCdata, CCdata$Country == "England")$MovedVoice == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
movedF <- round(sum (subset(CCdata, CCdata$Country == "England")$MovedFace == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
movedB <- round(sum (subset(CCdata, CCdata$Country == "England")$MovedBody == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
movedW <- round(sum (subset(CCdata, CCdata$Country == "England")$MovedWords == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
movedT <- round(sum (subset(CCdata, CCdata$Country == "England")$MovedTouch == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
movedO <- round(sum (subset(CCdata, CCdata$Country == "England")$MovedOther == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)

moved_percent_7 <- c(movedV, movedF, movedB, movedW, movedT, movedO)


#gratitude
gratV <- round(sum (subset(CCdata, CCdata$Country == "England")$GratitudeVoice == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
gratF <- round(sum (subset(CCdata, CCdata$Country == "England")$GratitudeFace == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
gratB <- round(sum (subset(CCdata, CCdata$Country == "England")$GratitudeBody == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
gratW <- round(sum (subset(CCdata, CCdata$Country == "England")$GratitudeWords == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
gratT <- round(sum (subset(CCdata, CCdata$Country == "England")$GratitudeTouch == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
gratO <- round(sum (subset(CCdata, CCdata$Country == "England")$GratitudeOther == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)

grat_percent_7 <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum (subset(CCdata, CCdata$Country == "England")$InterestVoice == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
interF <- round(sum (subset(CCdata, CCdata$Country == "England")$InterestFace == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
interB <- round(sum (subset(CCdata, CCdata$Country == "England")$InterestBody == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
interW <- round(sum (subset(CCdata, CCdata$Country == "England")$InterestWords == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
interT <- round(sum (subset(CCdata, CCdata$Country == "England")$InterestTouch == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
interO <- round(sum (subset(CCdata, CCdata$Country == "England")$InterestOther == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)

inter_percent_7 <- c(interV, interF, interB, interW, interT, interO)



#triumph
triV <- round(sum (subset(CCdata, CCdata$Country == "England")$TriumphVoice == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
triF <- round(sum (subset(CCdata, CCdata$Country == "England")$TriumphFace == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
triB <- round(sum (subset(CCdata, CCdata$Country == "England")$TriumphBody == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
triW <- round(sum (subset(CCdata, CCdata$Country == "England")$TriumphWords == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
triT <- round(sum (subset(CCdata, CCdata$Country == "England")$TriumphTouch == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)
triO <- round(sum (subset(CCdata, CCdata$Country == "England")$TriumphOther == 1) / nrow(subset(CCdata, CCdata$Country == "England")) *100, 2)

trium_percent_7 <- c(triV, triF, triB, triW, triT, triO)



# calculating 95% confidence intervals


#feeling moved
moved_proportions_7 <- moved_percent_7 / 100
moved_CI_7 <- sqrt(moved_proportions_7 * (1 - moved_proportions_7) / nrow(subset(CCdata, CCdata$Country == "England"))) * 1.96

#gratitude
grat_proportions_7 <- grat_percent_7 / 100
grat_CI_7 <- sqrt(grat_proportions_7 * (1 - grat_proportions_7) / nrow(subset(CCdata, CCdata$Country == "England"))) * 1.96

#interest
inter_proportions_7 <- inter_percent_7 / 100
inter_CI_7 <- sqrt(inter_proportions_7 * (1 - inter_proportions_7) / nrow(subset(CCdata, CCdata$Country == "England"))) * 1.96

#triumph
trium_proportions_7 <- trium_percent_7 / 100
trium_CI_7 <- sqrt(trium_proportions_7 * (1 - trium_proportions_7) / nrow(subset(CCdata, CCdata$Country == "England"))) * 1.96


#to see the CIs
round (moved_percent_7 - moved_CI_7 * 100, 2)
round (moved_percent_7 + moved_CI_7 * 100, 2)
round (grat_percent_7 - grat_CI_7 * 100, 2)
round (grat_percent_7 + grat_CI_7 * 100, 2)
round (inter_percent_7 - inter_CI_7 * 100, 2)
round (inter_percent_7 + inter_CI_7 * 100, 2)
round (trium_percent_7 - trium_CI_7 * 100, 2)
round (trium_percent_7 + trium_CI_7 * 100, 2)









#Germany


#feeling moved
movedV <- round(sum (subset(CCdata, CCdata$Country == "Germany")$MovedVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
movedF <- round(sum (subset(CCdata, CCdata$Country == "Germany")$MovedFace == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
movedB <- round(sum (subset(CCdata, CCdata$Country == "Germany")$MovedBody == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
movedW <- round(sum (subset(CCdata, CCdata$Country == "Germany")$MovedWords == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
movedT <- round(sum (subset(CCdata, CCdata$Country == "Germany")$MovedTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
movedO <- round(sum (subset(CCdata, CCdata$Country == "Germany")$MovedOther == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)

moved_percent_8 <- c(movedV, movedF, movedB, movedW, movedT, movedO)


#gratitude
gratV <- round(sum (subset(CCdata, CCdata$Country == "Germany")$GratitudeVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
gratF <- round(sum (subset(CCdata, CCdata$Country == "Germany")$GratitudeFace == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
gratB <- round(sum (subset(CCdata, CCdata$Country == "Germany")$GratitudeBody == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
gratW <- round(sum (subset(CCdata, CCdata$Country == "Germany")$GratitudeWords == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
gratT <- round(sum (subset(CCdata, CCdata$Country == "Germany")$GratitudeTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
gratO <- round(sum (subset(CCdata, CCdata$Country == "Germany")$GratitudeOther == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)

grat_percent_8 <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum (subset(CCdata, CCdata$Country == "Germany")$InterestVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
interF <- round(sum (subset(CCdata, CCdata$Country == "Germany")$InterestFace == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
interB <- round(sum (subset(CCdata, CCdata$Country == "Germany")$InterestBody == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
interW <- round(sum (subset(CCdata, CCdata$Country == "Germany")$InterestWords == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
interT <- round(sum (subset(CCdata, CCdata$Country == "Germany")$InterestTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
interO <- round(sum (subset(CCdata, CCdata$Country == "Germany")$InterestOther == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)

inter_percent_8 <- c(interV, interF, interB, interW, interT, interO)



#triumph
triV <- round(sum (subset(CCdata, CCdata$Country == "Germany")$TriumphVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
triF <- round(sum (subset(CCdata, CCdata$Country == "Germany")$TriumphFace == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
triB <- round(sum (subset(CCdata, CCdata$Country == "Germany")$TriumphBody == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
triW <- round(sum (subset(CCdata, CCdata$Country == "Germany")$TriumphWords == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
triT <- round(sum (subset(CCdata, CCdata$Country == "Germany")$TriumphTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)
triO <- round(sum (subset(CCdata, CCdata$Country == "Germany")$TriumphOther == 1) / nrow(subset(CCdata, CCdata$Country == "Germany")) *100, 2)

trium_percent_8 <- c(triV, triF, triB, triW, triT, triO)



# calculating 95% confidence intervals


#feeling moved
moved_proportions_8 <- moved_percent_8 / 100
moved_CI_8 <- sqrt(moved_proportions_8 * (1 - moved_proportions_8) / nrow(subset(CCdata, CCdata$Country == "Germany"))) * 1.96

#gratitude
grat_proportions_8 <- grat_percent_8 / 100
grat_CI_8 <- sqrt(grat_proportions_8 * (1 - grat_proportions_8) / nrow(subset(CCdata, CCdata$Country == "Germany"))) * 1.96

#interest
inter_proportions_8 <- inter_percent_8 / 100
inter_CI_8 <- sqrt(inter_proportions_8 * (1 - inter_proportions_8) / nrow(subset(CCdata, CCdata$Country == "Germany"))) * 1.96

#triumph
trium_proportions_8 <- trium_percent_8 / 100
trium_CI_8 <- sqrt(trium_proportions_8 * (1 - trium_proportions_8) / nrow(subset(CCdata, CCdata$Country == "Germany"))) * 1.96


#to see the CIs
round (moved_percent_8 - moved_CI_8 * 100, 2)
round (moved_percent_8 + moved_CI_8 * 100, 2)
round (grat_percent_8 - grat_CI_8 * 100, 2)
round (grat_percent_8 + grat_CI_8 * 100, 2)
round (inter_percent_8 - inter_CI_8 * 100, 2)
round (inter_percent_8 + inter_CI_8 * 100, 2)
round (trium_percent_8 - trium_CI_8 * 100, 2)
round (trium_percent_8 + trium_CI_8 * 100, 2)







#India


#feeling moved
movedV <- round(sum (subset(CCdata, CCdata$Country == "India")$MovedVoice == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
movedF <- round(sum (subset(CCdata, CCdata$Country == "India")$MovedFace == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
movedB <- round(sum (subset(CCdata, CCdata$Country == "India")$MovedBody == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
movedW <- round(sum (subset(CCdata, CCdata$Country == "India")$MovedWords == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
movedT <- round(sum (subset(CCdata, CCdata$Country == "India")$MovedTouch == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
movedO <- round(sum (subset(CCdata, CCdata$Country == "India")$MovedOther == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)

moved_percent_9 <- c(movedV, movedF, movedB, movedW, movedT, movedO)


#gratitude
gratV <- round(sum (subset(CCdata, CCdata$Country == "India")$GratitudeVoice == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
gratF <- round(sum (subset(CCdata, CCdata$Country == "India")$GratitudeFace == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
gratB <- round(sum (subset(CCdata, CCdata$Country == "India")$GratitudeBody == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
gratW <- round(sum (subset(CCdata, CCdata$Country == "India")$GratitudeWords == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
gratT <- round(sum (subset(CCdata, CCdata$Country == "India")$GratitudeTouch == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
gratO <- round(sum (subset(CCdata, CCdata$Country == "India")$GratitudeOther == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)

grat_percent_9 <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum (subset(CCdata, CCdata$Country == "India")$InterestVoice == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
interF <- round(sum (subset(CCdata, CCdata$Country == "India")$InterestFace == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
interB <- round(sum (subset(CCdata, CCdata$Country == "India")$InterestBody == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
interW <- round(sum (subset(CCdata, CCdata$Country == "India")$InterestWords == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
interT <- round(sum (subset(CCdata, CCdata$Country == "India")$InterestTouch == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
interO <- round(sum (subset(CCdata, CCdata$Country == "India")$InterestOther == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)

inter_percent_9 <- c(interV, interF, interB, interW, interT, interO)



#triumph
triV <- round(sum (subset(CCdata, CCdata$Country == "India")$TriumphVoice == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
triF <- round(sum (subset(CCdata, CCdata$Country == "India")$TriumphFace == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
triB <- round(sum (subset(CCdata, CCdata$Country == "India")$TriumphBody == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
triW <- round(sum (subset(CCdata, CCdata$Country == "India")$TriumphWords == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
triT <- round(sum (subset(CCdata, CCdata$Country == "India")$TriumphTouch == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)
triO <- round(sum (subset(CCdata, CCdata$Country == "India")$TriumphOther == 1) / nrow(subset(CCdata, CCdata$Country == "India")) *100, 2)

trium_percent_9 <- c(triV, triF, triB, triW, triT, triO)



# calculating 95% confidence intervals


#feeling moved
moved_proportions_9 <- moved_percent_9 / 100
moved_CI_9 <- sqrt(moved_proportions_9 * (1 - moved_proportions_9) / nrow(subset(CCdata, CCdata$Country == "India"))) * 1.96

#gratitude
grat_proportions_9 <- grat_percent_9 / 100
grat_CI_9 <- sqrt(grat_proportions_9 * (1 - grat_proportions_9) / nrow(subset(CCdata, CCdata$Country == "India"))) * 1.96

#interest
inter_proportions_9 <- inter_percent_9 / 100
inter_CI_9 <- sqrt(inter_proportions_9 * (1 - inter_proportions_9) / nrow(subset(CCdata, CCdata$Country == "India"))) * 1.96

#triumph
trium_proportions_9 <- trium_percent_9 / 100
trium_CI_9 <- sqrt(trium_proportions_9 * (1 - trium_proportions_9) / nrow(subset(CCdata, CCdata$Country == "India"))) * 1.96



#to see the CIs
round (moved_percent_9 - moved_CI_9 * 100, 2)
round (moved_percent_9 + moved_CI_9 * 100, 2)
round (grat_percent_9 - grat_CI_9 * 100, 2)
round (grat_percent_9 + grat_CI_9 * 100, 2)
round (inter_percent_9 - inter_CI_9 * 100, 2)
round (inter_percent_9 + inter_CI_9 * 100, 2)
round (trium_percent_9 - trium_CI_9 * 100, 2)
round (trium_percent_9 + trium_CI_9 * 100, 2)








#Netherlands


#feeling moved
movedV <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$MovedVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
movedF <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$MovedFace == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
movedB <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$MovedBody == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
movedW <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$MovedWords == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
movedT <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$MovedTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
movedO <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$MovedOther == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)

moved_percent_10 <- c(movedV, movedF, movedB, movedW, movedT, movedO)


#gratitude
gratV <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$GratitudeVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
gratF <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$GratitudeFace == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
gratB <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$GratitudeBody == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
gratW <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$GratitudeWords == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
gratT <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$GratitudeTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
gratO <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$GratitudeOther == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)

grat_percent_10 <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$InterestVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
interF <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$InterestFace == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
interB <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$InterestBody == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
interW <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$InterestWords == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
interT <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$InterestTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
interO <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$InterestOther == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)

inter_percent_10 <- c(interV, interF, interB, interW, interT, interO)



#triumph
triV <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$TriumphVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
triF <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$TriumphFace == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
triB <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$TriumphBody == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
triW <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$TriumphWords == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
triT <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$TriumphTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)
triO <- round(sum (subset(CCdata, CCdata$Country == "Netherlands")$TriumphOther == 1) / nrow(subset(CCdata, CCdata$Country == "Netherlands")) *100, 2)

trium_percent_10 <- c(triV, triF, triB, triW, triT, triO)



# calculating 95% confidence intervals


#feeling moved
moved_proportions_10 <- moved_percent_10 / 100
moved_CI_10 <- sqrt(moved_proportions_10 * (1 - moved_proportions_10) / nrow(subset(CCdata, CCdata$Country == "Netherlands"))) * 1.96

#gratitude
grat_proportions_10 <- grat_percent_10 / 100
grat_CI_10 <- sqrt(grat_proportions_10 * (1 - grat_proportions_10) / nrow(subset(CCdata, CCdata$Country == "Netherlands"))) * 1.96

#interest
inter_proportions_10 <- inter_percent_10 / 100
inter_CI_10 <- sqrt(inter_proportions_10 * (1 - inter_proportions_10) / nrow(subset(CCdata, CCdata$Country == "Netherlands"))) * 1.96

#triumph
trium_proportions_10 <- trium_percent_10 / 100
trium_CI_10 <- sqrt(trium_proportions_10 * (1 - trium_proportions_10) / nrow(subset(CCdata, CCdata$Country == "Netherlands"))) * 1.96



#to see the CIs
round (moved_percent_10 - moved_CI_10 * 100, 2)
round (moved_percent_10 + moved_CI_10 * 100, 2)
round (grat_percent_10 - grat_CI_10 * 100, 2)
round (grat_percent_10 + grat_CI_10 * 100, 2)
round (inter_percent_10 - inter_CI_10 * 100, 2)
round (inter_percent_10 + inter_CI_10 * 100, 2)
round (trium_percent_10 - trium_CI_10 * 100, 2)
round (trium_percent_10 + trium_CI_10 * 100, 2)










#Russia


#feeling moved
movedV <- round(sum (subset(CCdata, CCdata$Country == "Russia")$MovedVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
movedF <- round(sum (subset(CCdata, CCdata$Country == "Russia")$MovedFace == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
movedB <- round(sum (subset(CCdata, CCdata$Country == "Russia")$MovedBody == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
movedW <- round(sum (subset(CCdata, CCdata$Country == "Russia")$MovedWords == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
movedT <- round(sum (subset(CCdata, CCdata$Country == "Russia")$MovedTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
movedO <- round(sum (subset(CCdata, CCdata$Country == "Russia")$MovedOther == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)

moved_percent_11 <- c(movedV, movedF, movedB, movedW, movedT, movedO)


#gratitude
gratV <- round(sum (subset(CCdata, CCdata$Country == "Russia")$GratitudeVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
gratF <- round(sum (subset(CCdata, CCdata$Country == "Russia")$GratitudeFace == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
gratB <- round(sum (subset(CCdata, CCdata$Country == "Russia")$GratitudeBody == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
gratW <- round(sum (subset(CCdata, CCdata$Country == "Russia")$GratitudeWords == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
gratT <- round(sum (subset(CCdata, CCdata$Country == "Russia")$GratitudeTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
gratO <- round(sum (subset(CCdata, CCdata$Country == "Russia")$GratitudeOther == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)

grat_percent_11 <- c(gratV, gratF, gratB, gratW, gratT, gratO)


#interest
interV <- round(sum (subset(CCdata, CCdata$Country == "Russia")$InterestVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
interF <- round(sum (subset(CCdata, CCdata$Country == "Russia")$InterestFace == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
interB <- round(sum (subset(CCdata, CCdata$Country == "Russia")$InterestBody == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
interW <- round(sum (subset(CCdata, CCdata$Country == "Russia")$InterestWords == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
interT <- round(sum (subset(CCdata, CCdata$Country == "Russia")$InterestTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
interO <- round(sum (subset(CCdata, CCdata$Country == "Russia")$InterestOther == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)

inter_percent_11 <- c(interV, interF, interB, interW, interT, interO)



#triumph
triV <- round(sum (subset(CCdata, CCdata$Country == "Russia")$TriumphVoice == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
triF <- round(sum (subset(CCdata, CCdata$Country == "Russia")$TriumphFace == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
triB <- round(sum (subset(CCdata, CCdata$Country == "Russia")$TriumphBody == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
triW <- round(sum (subset(CCdata, CCdata$Country == "Russia")$TriumphWords == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
triT <- round(sum (subset(CCdata, CCdata$Country == "Russia")$TriumphTouch == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)
triO <- round(sum (subset(CCdata, CCdata$Country == "Russia")$TriumphOther == 1) / nrow(subset(CCdata, CCdata$Country == "Russia")) *100, 2)

trium_percent_11 <- c(triV, triF, triB, triW, triT, triO)



# calculating 95% confidence intervals


#feeling moved
moved_proportions_11 <- moved_percent_11 / 100
moved_CI_11 <- sqrt(moved_proportions_11 * (1 - moved_proportions_11) / nrow(subset(CCdata, CCdata$Country == "Russia"))) * 1.96

#gratitude
grat_proportions_11 <- grat_percent_11 / 100
grat_CI_11 <- sqrt(grat_proportions_11 * (1 - grat_proportions_11) / nrow(subset(CCdata, CCdata$Country == "Russia"))) * 1.96

#interest
inter_proportions_11 <- inter_percent_11 / 100
inter_CI_11 <- sqrt(inter_proportions_11 * (1 - inter_proportions_11) / nrow(subset(CCdata, CCdata$Country == "Russia"))) * 1.96

#triumph
trium_proportions_11 <- trium_percent_11 / 100
trium_CI_11 <- sqrt(trium_proportions_11 * (1 - trium_proportions_11) / nrow(subset(CCdata, CCdata$Country == "Russia"))) * 1.96



#to see the CIs
round (moved_percent_11 - moved_CI_11 * 100, 2)
round (moved_percent_11 + moved_CI_11 * 100, 2)
round (grat_percent_11 - grat_CI_11 * 100, 2)
round (grat_percent_11 + grat_CI_11 * 100, 2)
round (inter_percent_11 - inter_CI_11 * 100, 2)
round (inter_percent_11 + inter_CI_11 * 100, 2)
round (trium_percent_11 - trium_CI_11 * 100, 2)
round (trium_percent_11 + trium_CI_11 * 100, 2)










         ########### Within-country analysis ############


  ##### Preparing for Cochran's Q calculations

# Checking whether sample size is large enough - Procedure: the number of participants with all 1s and 0s cannot be too high, i.e., there must be at least n*k >= 24. k is 6 (number of modalities), so there need to be at least 4 participants who do not have all 1s (no one in the data set has all 0s).

nrow(subset(CCdata, CCdata$MovedFace == 1 & CCdata$MovedVoice == 1 & CCdata$MovedBody == 1  & CCdata$MovedTouch == 1 & CCdata$MovedOther == 1 & CCdata$MovedWords == 1))
nrow(subset(CCdata, CCdata$GratitudeFace == 1 & CCdata$GratitudeVoice == 1 & CCdata$GratitudeBody == 1 & CCdata$GratitudeTouch == 1 & CCdata$GratitudeOther == 1 & CCdata$GratitudeWords == 1))
nrow(subset(CCdata, CCdata$InterestFace == 1 & CCdata$InterestVoice == 1 & CCdata$InterestBody == 1 & CCdata$InterestTouch == 1 & CCdata$InterestOther == 1 & CCdata$InterestWords == 1))
nrow(subset(CCdata, CCdata$TriumphFace == 1 & CCdata$TriumphVoice == 1 & CCdata$TriumphBody == 1 & CCdata$TriumphTouch == 1 & CCdata$TriumphOther == 1 & CCdata$TriumphWords == 1))




############ Load long format Cross-cultural data and convert to factors ##########
CCdataLong <- read_spss("Cross_cultural_Study_2_Long.sav")

CCdataLong$Modalities <- factor(CCdataLong$Modalities, levels = unique(CCdataLong$Modalities)) 


    ####### Calculating Cochran's Q for each emotion, within each country


   #Australia

#feeling moved
cochran.qtest(Moved ~ Modalities | pp,     
              data = subset(CCdataLong, CCdataLong$Country == "Australia"))
pairwiseMcnemar(Moved ~ Modalities | pp,     
                            data   = subset(CCdataLong, CCdataLong$Country == "Australia"),
                            test   = "mcnemar",
                            method = "bonferroni",
                            digits = 7)

#effect size
austr <- subset(CCdata, CCdata$Country == "Australia")
Moved_matrix_austr <- cbind(austr$MovedVoice, austr$MovedFace, austr$MovedBody, austr$MovedWords, austr$MovedTouch, austr$MovedOther)

cQ_effectsize(Moved_matrix_austr)
check_ft(Moved_matrix_austr)  



#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Australia"))
pairwiseMcnemar(Gratitude ~ Modalities | pp,
                           data   = subset(CCdataLong, CCdataLong$Country == "Australia"),
                           test   = "mcnemar",
                           method = "bonferroni",
                           digits = 7)
  #effect size
Grat_matrix_austr <- cbind(austr$GratitudeVoice, austr$GratitudeFace, austr$GratitudeBody, austr$GratitudeWords, austr$GratitudeTouch, austr$GratitudeOther)

cQ_effectsize(Grat_matrix_austr)
check_ft(Grat_matrix_austr)



#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Australia"))
pairwiseMcnemar(Interest ~ Modalities | pp,
                            data   = subset(CCdataLong, CCdataLong$Country == "Australia"),
                            test   = "mcnemar",
                            method = "bonferroni",
                            digits = 7)     
  #effect size
Inter_matrix_austr <- cbind(austr$InterestVoice, austr$InterestFace, austr$InterestBody, austr$InterestWords, austr$InterestTouch, austr$InterestOther)

cQ_effectsize(Inter_matrix_austr)
check_ft(Inter_matrix_austr) 




#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Australia"))
pairwiseMcnemar(Triumph ~ Modalities | pp,
                            data   = subset(CCdataLong, CCdataLong$Country == "Australia"),
                            test   = "mcnemar",
                            method = "bonferroni",
                            digits = 7)                            
  #effect size
Trium_matrix_austr <- cbind(austr$TriumphVoice, austr$TriumphFace, austr$TriumphBody, austr$TriumphWords, austr$TriumphTouch, austr$TriumphOther)

cQ_effectsize(Trium_matrix_austr)
check_ft(Trium_matrix_austr) 






    #Austria
#feeling moved
cochran.qtest(Moved ~ Modalities | pp,    
              data = subset(CCdataLong, CCdataLong$Country == "Austria"))
pairwiseMcnemar(Moved ~ Modalities | pp,     
                data   = subset(CCdataLong, CCdataLong$Country == "Austria"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
aus <- subset(CCdata, CCdata$Country == "Austria")
Moved_matrix_aus <- cbind(aus$MovedVoice, aus$MovedFace, aus$MovedBody, aus$MovedWords, aus$MovedTouch, aus$MovedOther)
cQ_effectsize(Moved_matrix_aus)
check_ft(Moved_matrix_aus)


#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Austria"))
pairwiseMcnemar(Gratitude ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Austria"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
Grat_matrix_aus <- cbind(aus$GratitudeVoice, aus$GratitudeFace, aus$GratitudeBody, aus$GratitudeWords, aus$GratitudeTouch, aus$GratitudeOther)
cQ_effectsize(Grat_matrix_aus)
check_ft(Grat_matrix_aus)


#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Austria"))
pairwiseMcnemar(Interest ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Austria"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)  
#effect size
Inter_matrix_aus <- cbind(aus$InterestVoice, aus$InterestFace, aus$InterestBody, aus$InterestWords, aus$InterestTouch, aus$InterestOther)
cQ_effectsize(Inter_matrix_aus)
check_ft(Inter_matrix_aus)




#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Austria"))
pairwiseMcnemar(Triumph ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Austria"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)    
#effect size
Trium_matrix_aus <- cbind(aus$TriumphVoice, aus$TriumphFace, aus$TriumphBody, aus$TriumphWords, aus$TriumphTouch, aus$TriumphOther)
cQ_effectsize(Trium_matrix_aus)
check_ft(Trium_matrix_aus)




    #Canada
#feeling moved
cochran.qtest(Moved ~ Modalities | pp,     
              data = subset(CCdataLong, CCdataLong$Country == "Canada"))
pairwiseMcnemar(Moved ~ Modalities | pp,     
                data   = subset(CCdataLong, CCdataLong$Country == "Canada"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
can <- subset(CCdata, CCdata$Country == "Canada")
Moved_matrix_can <- cbind(can$MovedVoice, can$MovedFace, can$MovedBody, can$MovedWords, can$MovedTouch, can$MovedOther)
cQ_effectsize(Moved_matrix_can)
check_ft(Moved_matrix_can)



#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Canada"))
pairwiseMcnemar(Gratitude ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Canada"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
Grat_matrix_can <- cbind(can$GratitudeVoice, can$GratitudeFace, can$GratitudeBody, can$GratitudeWords, can$GratitudeTouch, can$GratitudeOther)
cQ_effectsize(Grat_matrix_can)
check_ft(Grat_matrix_can)



#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Canada"))
pairwiseMcnemar(Interest ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Canada"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)        
#effect size
Inter_matrix_can <- cbind(can$InterestVoice, can$InterestFace, can$InterestBody, can$InterestWords, can$InterestTouch, can$InterestOther)
cQ_effectsize(Inter_matrix_can)
check_ft(Inter_matrix_can) 




#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Canada"))
pairwiseMcnemar(Triumph ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Canada"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)

#effect size
Trium_matrix_can <- cbind(can$TriumphVoice, can$TriumphFace, can$TriumphBody, can$TriumphWords, can$TriumphTouch, can$TriumphOther)
cQ_effectsize(Trium_matrix_can)
check_ft(Trium_matrix_can)






    #China
#feeling moved
cochran.qtest(Moved ~ Modalities | pp,     
              data = subset(CCdataLong, CCdataLong$Country == "China"))

pairwiseMcnemar(Moved ~ Modalities | pp,     
                data   = subset(CCdataLong, CCdataLong$Country == "China"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
chin <- subset(CCdata, CCdata$Country == "China")
Moved_matrix_chin <- cbind(chin$MovedVoice, chin$MovedFace, chin$MovedBody, chin$MovedWords, chin$MovedTouch, chin$MovedOther)

cQ_effectsize(Moved_matrix_chin)
check_ft(Moved_matrix_chin)


#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "China"))
pairwiseMcnemar(Gratitude ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "China"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
  #effect size
Grat_matrix_chin <- cbind(chin$GratitudeVoice, chin$GratitudeFace, chin$GratitudeBody, chin$GratitudeWords, chin$GratitudeTouch, chin$GratitudeOther)

cQ_effectsize(Grat_matrix_chin)
check_ft(Grat_matrix_chin)


#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "China"))
pairwiseMcnemar(Interest ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "China"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)   



  #effect size
Inter_matrix_chin <- cbind(chin$InterestVoice, chin$InterestFace, chin$InterestBody, chin$InterestWords, chin$InterestTouch, chin$InterestOther)

cQ_effectsize(Inter_matrix_chin)
check_ft(Inter_matrix_chin)



#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "China"))
pairwiseMcnemar(Triumph ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "China"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7) 

#effect size
Trium_matrix_chin <- cbind(chin$TriumphVoice, chin$TriumphFace, chin$TriumphBody, chin$TriumphWords, chin$TriumphTouch, chin$TriumphOther)

cQ_effectsize(Trium_matrix_chin)
check_ft(Trium_matrix_chin)







    #Croatia
#feeling moved
cochran.qtest(Moved ~ Modalities | pp,     
              data = subset(CCdataLong, CCdataLong$Country == "Croatia"))

pairwiseMcnemar(Moved ~ Modalities | pp,     
                data   = subset(CCdataLong, CCdataLong$Country == "Croatia"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
cro <- subset(CCdata, CCdata$Country == "Croatia")
Moved_matrix_cro <- cbind(cro$MovedVoice, cro$MovedFace, cro$MovedBody, cro$MovedWords, cro$MovedTouch, cro$MovedOther)
cQ_effectsize(Moved_matrix_cro)
check_ft(Moved_matrix_cro)



#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Croatia"))
pairwiseMcnemar(Gratitude ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Croatia"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
Grat_matrix_cro <- cbind(cro$GratitudeVoice, cro$GratitudeFace, cro$GratitudeBody, cro$GratitudeWords, cro$GratitudeTouch, cro$GratitudeOther)
cQ_effectsize(Grat_matrix_cro)
check_ft(Grat_matrix_cro)



#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Croatia"))
pairwiseMcnemar(Interest ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Croatia"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)  
#effect size
Inter_matrix_cro <- cbind(cro$InterestVoice, cro$InterestFace, cro$InterestBody, cro$InterestWords, cro$InterestTouch, cro$InterestOther)
cQ_effectsize(Inter_matrix_cro)
check_ft(Inter_matrix_cro)





#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Croatia"))
pairwiseMcnemar(Triumph ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Croatia"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)    
#effect size
Trium_matrix_cro <- cbind(cro$TriumphVoice, cro$TriumphFace, cro$TriumphBody, cro$TriumphWords, cro$TriumphTouch, cro$TriumphOther)
cQ_effectsize(Trium_matrix_cro)
check_ft(Trium_matrix_cro)








    #England
#feeling moved
cochran.qtest(Moved ~ Modalities | pp,     
              data = subset(CCdataLong, CCdataLong$Country == "England"))

pairwiseMcnemar(Moved ~ Modalities | pp,     
                data   = subset(CCdataLong, CCdataLong$Country == "England"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)

#effect size
engl <- subset(CCdata, CCdata$Country == "England")
Moved_matrix_engl <- cbind(engl$MovedVoice, engl$MovedFace, engl$MovedBody, engl$MovedWords, engl$MovedTouch, engl$MovedOther)

cQ_effectsize(Moved_matrix_engl)
check_ft(Moved_matrix_engl)



#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "England"))
pairwiseMcnemar(Gratitude ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "England"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
Grat_matrix_engl <- cbind(engl$GratitudeVoice, engl$GratitudeFace, engl$GratitudeBody, engl$GratitudeWords, engl$GratitudeTouch, engl$GratitudeOther)

cQ_effectsize(Grat_matrix_engl)
check_ft(Grat_matrix_engl)




#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "England"))
pairwiseMcnemar(Interest ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "England"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)    
#effect size
Inter_matrix_engl <- cbind(engl$InterestVoice, engl$InterestFace, engl$InterestBody, engl$InterestWords, engl$InterestTouch, engl$InterestOther)

cQ_effectsize(Inter_matrix_engl)
check_ft(Inter_matrix_engl)






#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "England"))
pairwiseMcnemar(Triumph ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "England"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)                            
#effect size
Trium_matrix_engl <- cbind(engl$TriumphVoice, engl$TriumphFace, engl$TriumphBody, engl$TriumphWords, engl$TriumphTouch, engl$TriumphOther)

cQ_effectsize(Trium_matrix_engl)
check_ft(Trium_matrix_engl)









    #Germany
#feeling moved
cochran.qtest(Moved ~ Modalities | pp,     
              data = subset(CCdataLong, CCdataLong$Country == "Germany"))

pairwiseMcnemar(Moved ~ Modalities | pp,     
                data   = subset(CCdataLong, CCdataLong$Country == "Germany"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)

#effect size
germ <- subset(CCdata, CCdata$Country == "Germany")
Moved_matrix_germ <- cbind(germ$MovedVoice, germ$MovedFace, germ$MovedBody, germ$MovedWords, germ$MovedTouch, germ$MovedOther)

cQ_effectsize(Moved_matrix_germ)
check_ft(Moved_matrix_germ)




#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Germany"))
pairwiseMcnemar(Gratitude ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Germany"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
Grat_matrix_germ <- cbind(germ$GratitudeVoice, germ$GratitudeFace, germ$GratitudeBody, germ$GratitudeWords, germ$GratitudeTouch, germ$GratitudeOther)

cQ_effectsize(Grat_matrix_germ)
check_ft(Grat_matrix_germ)



#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Germany"))
pairwiseMcnemar(Interest ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Germany"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)                         
#effect size
Inter_matrix_germ <- cbind(germ$InterestVoice, germ$InterestFace, germ$InterestBody, germ$InterestWords, germ$InterestTouch, germ$InterestOther)

cQ_effectsize(Inter_matrix_germ)
check_ft(Inter_matrix_germ)





#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Germany"))
pairwiseMcnemar(Triumph ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Germany"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7) 


#effect size
Trium_matrix_germ <- cbind(germ$TriumphVoice, germ$TriumphFace, germ$TriumphBody, germ$TriumphWords, germ$TriumphTouch, germ$TriumphOther)

cQ_effectsize(Trium_matrix_germ)
check_ft(Trium_matrix_germ)









    #India
#feeling moved
cochran.qtest(Moved ~ Modalities | pp,     
              data = subset(CCdataLong, CCdataLong$Country == "India"))

pairwiseMcnemar(Moved ~ Modalities | pp,     
                data   = subset(CCdataLong, CCdataLong$Country == "India"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
ind <- subset(CCdata, CCdata$Country == "India")
Moved_matrix_ind <- cbind(ind$MovedVoice, ind$MovedFace, ind$MovedBody, ind$MovedWords, ind$MovedTouch, ind$MovedOther)

cQ_effectsize(Moved_matrix_ind)
check_ft(Moved_matrix_ind)





#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "India"))
pairwiseMcnemar(Gratitude ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "India"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
Grat_matrix_ind <- cbind(ind$GratitudeVoice, ind$GratitudeFace, ind$GratitudeBody, ind$GratitudeWords, ind$GratitudeTouch, ind$GratitudeOther)

cQ_effectsize(Grat_matrix_ind)
check_ft(Grat_matrix_ind)



#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "India"))
pairwiseMcnemar(Interest ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "India"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7) 
#effect size
Inter_matrix_ind <- cbind(ind$InterestVoice, ind$InterestFace, ind$InterestBody, ind$InterestWords, ind$InterestTouch, ind$InterestOther)

cQ_effectsize(Inter_matrix_ind)
check_ft(Inter_matrix_ind)  



#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "India"))
pairwiseMcnemar(Triumph ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "India"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7) 
#effect size
Trium_matrix_ind <- cbind(ind$TriumphVoice, ind$TriumphFace, ind$TriumphBody, ind$TriumphWords, ind$TriumphTouch, ind$TriumphOther)

cQ_effectsize(Trium_matrix_ind)
check_ft(Trium_matrix_ind)









    #Netherlands
#feeling moved
cochran.qtest(Moved ~ Modalities | pp,     
              data = subset(CCdataLong, CCdataLong$Country == "Netherlands"))

pairwiseMcnemar(Moved ~ Modalities | pp,     
                data   = subset(CCdataLong, CCdataLong$Country == "Netherlands"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
nl <- subset(CCdata, CCdata$Country == "Netherlands")
Moved_matrix_nl <- cbind(nl$MovedVoice, nl$MovedFace, nl$MovedBody, nl$MovedWords, nl$MovedTouch, nl$MovedOther)
cQ_effectsize(Moved_matrix_nl)
check_ft(Moved_matrix_nl)


#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Netherlands"))
pairwiseMcnemar(Gratitude ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Netherlands"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
Grat_matrix_nl <- cbind(nl$GratitudeVoice, nl$GratitudeFace, nl$GratitudeBody, nl$GratitudeWords, nl$GratitudeTouch, nl$GratitudeOther)
cQ_effectsize(Grat_matrix_nl)
check_ft(Grat_matrix_nl)


#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Netherlands"))
pairwiseMcnemar(Interest ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Netherlands"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)  
#effect size
Inter_matrix_nl <- cbind(nl$InterestVoice, nl$InterestFace, nl$InterestBody, nl$InterestWords, nl$InterestTouch, nl$InterestOther)
cQ_effectsize(Inter_matrix_nl)
check_ft(Inter_matrix_nl)




#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Netherlands"))
pairwiseMcnemar(Triumph ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Netherlands"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)  
#effect size
Trium_matrix_nl <- cbind(nl$TriumphVoice, nl$TriumphFace, nl$TriumphBody, nl$TriumphWords, nl$TriumphTouch, nl$TriumphOther)
cQ_effectsize(Trium_matrix_nl)
check_ft(Trium_matrix_nl)









    #Russia
#feeling moved
cochran.qtest(Moved ~ Modalities | pp,     
              data = subset(CCdataLong, CCdataLong$Country == "Russia"))

pairwiseMcnemar(Moved ~ Modalities | pp,     
                data   = subset(CCdataLong, CCdataLong$Country == "Russia"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
rus <- subset(CCdata, CCdata$Country == "Russia")
Moved_matrix_rus <- cbind(rus$MovedVoice, rus$MovedFace, rus$MovedBody, rus$MovedWords, rus$MovedTouch, rus$MovedOther)
cQ_effectsize(Moved_matrix_rus)
check_ft(Moved_matrix_rus)





#gratitude
cochran.qtest(Gratitude ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Russia"))
pairwiseMcnemar(Gratitude ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Russia"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)
#effect size
Grat_matrix_rus <- cbind(rus$GratitudeVoice, rus$GratitudeFace, rus$GratitudeBody, rus$GratitudeWords, rus$GratitudeTouch, rus$GratitudeOther)
cQ_effectsize(Grat_matrix_rus)
check_ft(Grat_matrix_rus)




#interest
cochran.qtest(Interest ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Russia"))
pairwiseMcnemar(Interest ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Russia"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)   
#effect size
Inter_matrix_rus <- cbind(rus$InterestVoice, rus$InterestFace, rus$InterestBody, rus$InterestWords, rus$InterestTouch, rus$InterestOther)
cQ_effectsize(Inter_matrix_rus)
check_ft(Inter_matrix_rus)




#triumph
cochran.qtest(Triumph ~ Modalities | pp,
              data = subset(CCdataLong, CCdataLong$Country == "Russia"))
pairwiseMcnemar(Triumph ~ Modalities | pp,
                data   = subset(CCdataLong, CCdataLong$Country == "Russia"),
                test   = "mcnemar",
                method = "bonferroni",
                digits = 7)    
#effect size
Trium_matrix_rus <- cbind(rus$TriumphVoice, rus$TriumphFace, rus$TriumphBody, rus$TriumphWords, rus$TriumphTouch, rus$TriumphOther)
cQ_effectsize(Trium_matrix_rus)
check_ft(Trium_matrix_rus)















        ########### Overall cross-cultural descriptives and demographics (on entire dataset)

all_dataset1 <- CCdata

sum(all_dataset1$Gender==1)/nrow(all_dataset1)
min(all_dataset1$Age)
max(all_dataset1$Age)
mean(all_dataset1$Age)
sd(all_dataset1$Age)


#moved
movedV <- round(sum(all_dataset1$MovedVoice == 1) / nrow(all_dataset1) *100, 2)
movedF <- round(sum(all_dataset1$MovedFace == 1) / nrow(all_dataset1) *100, 2)
movedB <- round(sum(all_dataset1$MovedBody == 1) / nrow(all_dataset1) *100, 2)
movedW <- round(sum(all_dataset1$MovedWords == 1) / nrow(all_dataset1) *100, 2)
movedT <- round(sum(all_dataset1$MovedTouch == 1) / nrow(all_dataset1) *100, 2)
movedO <- round(sum(all_dataset1$MovedOther == 1) / nrow(all_dataset1) *100, 2)

moved_percent_overall <- c(movedV, movedF, movedB, movedW, movedT, movedO)

moved_proportions_overall <- moved_percent_overall / 100
moved_CI_overall <- sqrt(moved_proportions_overall * (1 - moved_proportions_overall) / nrow(all_dataset1)) * 1.96
round (moved_percent_overall - moved_CI_overall * 100, 2)
round (moved_percent_overall + moved_CI_overall * 100, 2)


#gratitude
gratV <- round(sum(all_dataset1$GratitudeVoice == 1) / nrow(all_dataset1) *100, 2)
gratF <- round(sum(all_dataset1$GratitudeFace == 1) / nrow(all_dataset1) *100, 2)
gratB <- round(sum(all_dataset1$GratitudeBody == 1) / nrow(all_dataset1) *100, 2)
gratW <- round(sum(all_dataset1$GratitudeWords == 1) / nrow(all_dataset1) *100, 2)
gratT <- round(sum(all_dataset1$GratitudeTouch == 1) / nrow(all_dataset1) *100, 2)
gratO <- round(sum(all_dataset1$GratitudeOther == 1) / nrow(all_dataset1) *100, 2)

grat_percent_overall <- c(gratV, gratF, gratB, gratW, gratT, gratO)

grat_proportions_overall <- grat_percent_overall / 100
grat_CI_overall <- sqrt(grat_proportions_overall * (1 - grat_proportions_overall) / nrow(all_dataset1)) * 1.96
round (grat_percent_overall - grat_CI_overall * 100, 2)
round (grat_percent_overall + grat_CI_overall * 100, 2)


#interest
interV <- round(sum(all_dataset1$InterestVoice == 1) / nrow(all_dataset1) *100, 2)
interF <- round(sum(all_dataset1$InterestFace == 1) / nrow(all_dataset1) *100, 2)
interB <- round(sum(all_dataset1$InterestBody == 1) / nrow(all_dataset1) *100, 2)
interW <- round(sum(all_dataset1$InterestWords == 1) / nrow(all_dataset1) *100, 2)
interT <- round(sum(all_dataset1$InterestTouch == 1) / nrow(all_dataset1) *100, 2)
interO <- round(sum(all_dataset1$InterestOther == 1) / nrow(all_dataset1) *100, 2)

inter_percent_overall <- c(interV, interF, interB, interW, interT, interO)

inter_proportions_overall <- inter_percent_overall / 100
inter_CI_overall <- sqrt(inter_proportions_overall * (1 - inter_proportions_overall) / nrow(all_dataset1)) * 1.96
round (inter_percent_overall - inter_CI_overall * 100, 2)
round (inter_percent_overall + inter_CI_overall * 100, 2)




#triumph
triV <- round(sum(all_dataset1$TriumphVoice == 1) / nrow(all_dataset1) *100, 2)
triF <- round(sum(all_dataset1$TriumphFace == 1) / nrow(all_dataset1) *100, 2)
triB <- round(sum(all_dataset1$TriumphBody == 1) / nrow(all_dataset1) *100, 2)
triW <- round(sum(all_dataset1$TriumphWords == 1) / nrow(all_dataset1) *100, 2)
triT <- round(sum(all_dataset1$TriumphTouch == 1) / nrow(all_dataset1) *100, 2)
triO <- round(sum(all_dataset1$TriumphOther == 1) / nrow(all_dataset1) *100, 2)

tri_percent_overall <- c(triV, triF, triB, triW, triT, triO)

tri_proportions_overall <- tri_percent_overall / 100
tri_CI_overall <- sqrt(tri_proportions_overall * (1 - tri_proportions_overall) / nrow(all_dataset1)) * 1.96
round (tri_percent_overall - tri_CI_overall * 100, 2)
round (tri_percent_overall + tri_CI_overall * 100, 2)










        ############### Cross-cultural comparison using frequentist chi-square approach (see JASP file for Bayesian)

p.values <- numeric (length = 20)

p.values[1] <- chisq.test(table (CCdata$Country, CCdata$MovedVoice))$p.value
cramerV(table (CCdata$Country, CCdata$MovedVoice), digits = 6)
p.values[2] <- chisq.test(table (CCdata$Country, CCdata$MovedFace))$p.value
cramerV(table (CCdata$Country, CCdata$MovedFace), digits = 6)
p.values[3] <- chisq.test(table (CCdata$Country, CCdata$MovedBody))$p.value
cramerV(table (CCdata$Country, CCdata$MovedBody), digits = 6)
p.values[4] <- chisq.test(table (CCdata$Country, CCdata$MovedWords))$p.value
cramerV(table (CCdata$Country, CCdata$MovedWords), digits = 6)
p.values[5] <- chisq.test(table (CCdata$Country, CCdata$MovedTouch))$p.value
cramerV(table (CCdata$Country, CCdata$MovedTouch), digits = 6)

p.values[6] <- chisq.test(table (CCdata$Country, CCdata$GratitudeVoice))$p.value
cramerV(table (CCdata$Country, CCdata$GratitudeVoice), digits = 6)
p.values[7] <- chisq.test(table (CCdata$Country, CCdata$GratitudeFace))$p.value
cramerV(table (CCdata$Country, CCdata$GratitudeFace), digits = 6)
p.values[8] <- chisq.test(table (CCdata$Country, CCdata$GratitudeBody))$p.value
cramerV(table (CCdata$Country, CCdata$GratitudeBody), digits = 6)
p.values[9] <- chisq.test(table (CCdata$Country, CCdata$GratitudeWords))$p.value
cramerV(table (CCdata$Country, CCdata$GratitudeWords), digits = 6)
p.values[10] <- chisq.test(table (CCdata$Country, CCdata$GratitudeTouch))$p.value
cramerV(table (CCdata$Country, CCdata$GratitudeTouch), digits = 6)

p.values[11] <- chisq.test(table (CCdata$Country, CCdata$InterestVoice))$p.value
cramerV(table (CCdata$Country, CCdata$InterestVoice), digits = 6)
p.values[12] <- chisq.test(table (CCdata$Country, CCdata$InterestFace))$p.value
cramerV(table (CCdata$Country, CCdata$InterestFace), digits = 6)
p.values[13] <- chisq.test(table (CCdata$Country, CCdata$InterestBody))$p.value
cramerV(table (CCdata$Country, CCdata$InterestBody), digits = 6)
p.values[14] <- chisq.test(table (CCdata$Country, CCdata$InterestWords))$p.value
cramerV(table (CCdata$Country, CCdata$InterestWords), digits = 6)
p.values[15] <- chisq.test(table (CCdata$Country, CCdata$InterestTouch))$p.value
cramerV(table (CCdata$Country, CCdata$InterestTouch), digits = 6)

p.values[16] <- chisq.test(table (CCdata$Country, CCdata$TriumphVoice))$p.value
cramerV(table (CCdata$Country, CCdata$TriumphVoice), digits = 6)
p.values[17] <- chisq.test(table (CCdata$Country, CCdata$TriumphFace))$p.value
cramerV(table (CCdata$Country, CCdata$TriumphFace), digits = 6)
p.values[18] <- chisq.test(table (CCdata$Country, CCdata$TriumphBody))$p.value
cramerV(table (CCdata$Country, CCdata$TriumphBody), digits = 6)
p.values[19] <- chisq.test(table (CCdata$Country, CCdata$TriumphWords))$p.value
cramerV(table (CCdata$Country, CCdata$TriumphWords), digits = 6)
p.values[20] <- chisq.test(table (CCdata$Country, CCdata$TriumphTouch))$p.value
cramerV(table (CCdata$Country, CCdata$TriumphTouch), digits = 6)

round(p.adjust(p = p.values, method = "bonferroni"), 3)






      ########### Creating plots of residuals for cross-cultural frequentist chi-square tests

# (Each plot was saved to pdf and inserted into Word for ESM from there.)

# library(vcd)


# lnames <- list (MovedVoice = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + MovedVoice,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Voice", set_labels = lnames,
#        labeling_args = list(set_varnames = c(MovedVoice = "Modality Selected")))
# dev.off()
# 
# 
# 
# lnames <- list (MovedFace = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + MovedFace,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Face", set_labels = lnames,
#        labeling_args = list(set_varnames = c(MovedFace = "Modality Selected")))
# dev.off()
# 
# 
# 
# lnames <- list (MovedBody = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + MovedBody,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Body", set_labels = lnames,
#        labeling_args = list(set_varnames = c(MovedBody = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (MovedWords = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + MovedWords,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Words", set_labels = lnames,
#        labeling_args = list(set_varnames = c(MovedWords = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (MovedTouch = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + MovedTouch,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Touch", set_labels = lnames,
#        labeling_args = list(set_varnames = c(MovedTouch = "Modality Selected")))
# dev.off()
# 
# 
# 
# lnames <- list (GratitudeVoice = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + GratitudeVoice,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Voice", set_labels = lnames,
#        labeling_args = list(set_varnames = c(GratitudeVoice = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (GratitudeFace = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + GratitudeFace,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Face", set_labels = lnames,
#        labeling_args = list(set_varnames = c(GratitudeFace = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (GratitudeBody = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + GratitudeBody,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Body", set_labels = lnames,
#        labeling_args = list(set_varnames = c(GratitudeBody = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (GratitudeWords = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + GratitudeWords,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Words", set_labels = lnames,
#        labeling_args = list(set_varnames = c(GratitudeWords = "Modality Selected")))
# dev.off()
# 
# 
# 
# lnames <- list (GratitudeTouch = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + GratitudeTouch,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Touch", set_labels = lnames,
#        labeling_args = list(set_varnames = c(GratitudeTouch = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (InterestVoice = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + InterestVoice,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Voice", set_labels = lnames,
#        labeling_args = list(set_varnames = c(InterestVoice = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (InterestFace = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + InterestFace,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Face", set_labels = lnames,
#        labeling_args = list(set_varnames = c(InterestFace = "Modality Selected")))
# dev.off()
# 
# 
# 
# lnames <- list (InterestBody = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + InterestBody,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Body", set_labels = lnames,
#        labeling_args = list(set_varnames = c(InterestBody = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (InterestWords = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + InterestWords,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Words", set_labels = lnames,
#        labeling_args = list(set_varnames = c(InterestWords = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (InterestTouch = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + InterestTouch,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Touch", set_labels = lnames,
#        labeling_args = list(set_varnames = c(InterestTouch = "Modality Selected")))
# dev.off()
# 
# 
# 
# lnames <- list (TriumphVoice = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + TriumphVoice,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Voice", set_labels = lnames,
#        labeling_args = list(set_varnames = c(TriumphVoice = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (TriumphFace = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + TriumphFace,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Face", set_labels = lnames,
#        labeling_args = list(set_varnames = c(TriumphFace = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (TriumphBody = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + TriumphBody,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Body", set_labels = lnames,
#        labeling_args = list(set_varnames = c(TriumphBody = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (TriumphWords = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + TriumphWords,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Words", set_labels = lnames,
#        labeling_args = list(set_varnames = c(TriumphWords = "Modality Selected")))
# dev.off()
# 
# 
# lnames <- list (TriumphTouch = c ("No", "Yes"), 
#                 Country = c ("AU", "AT", "CA", "CN", "HR", "EN", "DE", "IN", "NL", "RU"))
# 
# pdf("hello.pdf", height = 9, width = 25)
# mosaic(~ Country + TriumphTouch,
#        direction = c("v", "h"),                 
#        data = CCdata,
#        shade = TRUE, legend = TRUE,
#        main = "Touch", set_labels = lnames,
#        labeling_args = list(set_varnames = c(TriumphTouch = "Modality Selected")))
# dev.off()