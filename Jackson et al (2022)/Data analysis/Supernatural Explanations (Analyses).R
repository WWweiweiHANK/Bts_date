#' Code for Paper "Supernatural Explanations Are More Common For Natural Phenomena Than Social Phenomena"
#' Code Assembled by Joshua Conrad Jackson, Danica Wilbanks, and Joseph Watts
#' Contact joshua.jackson@kellogg.northwestern.edu with any questions

# Packages involved in data analysis
library(dplyr)
library(ggplot2)
library(ape)
library(caper)
library(phytools)
library(MASS)
library(phylolm)
library(reshape2)
library(rstudioapi) 
library(interactions)

# Set working directory to source folder
rm(list=ls())
setwd(dirname(getActiveDocumentContext()$path))

# Updating tiplabels function with a version that allows tips to be aligned
source("tiplabel_update.R")

# Read in the dataset that was written out at the end of the "cleaning" script
d <- read.csv("Processed_data Revision.csv") %>%
  as.data.frame()

#' Comment out this line for the full set of 114 societies (which is presented 
#' in the supplement)
#' Include the line to run analyses for the truncated sample (without societies 
#' with high borrowing)
# d <- d[d$New==0, ] 

#### Main Text, Methods: Set up the Phylogeny ####

setwd("../Data cleaning/") # Ensures that trees are read in from adjacent "data cleaning" folder
glot_tree1 <- read.nexus("Glot_tree1.tree")
glot_tree2 <- read.nexus("Glot_tree2.tree")
glot_tree3 <- read.nexus("Glot_tree3.tree")
jaeger_tree <- read.tree("Jaeger_Tree_Pruned.tree")

# Defining the tree to use (use tree object name as a character string)
# The Jaeger tree was our primary tree
# The reason for the complicated if else statements is that there are
# ...different taxa column names for different trees
tree_to_use <- "jaeger_tree"

if(tree_to_use == "glot_tree1"){
  
  tree <- glot_tree1
  d$tree_tips <- d$glottocode
  
}else if(tree_to_use == "glot_tree2"){
  
  tree <- glot_tree2
  d$tree_tips <- d$glottocode
  
}else if(tree_to_use == "glot_tree3"){
  
  tree <- glot_tree3
  d$tree_tips <- d$glottocode
  
}else if(tree_to_use == "jaeger_tree"){
  
  tree <- jaeger_tree
  d$tree_tips <- d$Jaeger_taxa_name
  
}

row.names(d) <- d$tree_tips

plot(tree)

# Dropping tips from tree that are not in the dataset
tips_to_prune <- tree$tip.label[!tree$tip.label %in% row.names(d)]
tree <- drop.tip(tree, tips_to_prune)

# Quick check of overlap
length(tree$tip.label) == nrow(d)
tree$tip.label %in% d$tree_tips %>% table()
d$tree_tips %in% tree$tip.label %>% table()
# These should all be True

# Reordering data to match order of tips on tree
d <- d[tree$tip.label, ]

# Simplifying tree structure, otherwise there are issues with the phylo.d function
tree <- collapse.singles(tree)
plot(tree)

# Remove node labels
tree$node.label <- NULL

#### Main Text, Methods: Test For Phylogenetic Structure #### 

# Define variable to use
d_var <- "V_1s_common"

# Note: The dataframe cannot have any missing data for phylo.d to function properly. 
# Best to create a temporary dataframe for the purpose.
d_bin <- d[,c("Culture",
              "tree_tips", 
              d_var)]

d_bin <- na.omit(d_bin) 
colnames(d_bin)[3] <- "d_var"

# Converting to a comparative data object
comp_d <- comparative.data(phy = tree,
                           data = d_bin,
                           names.col = tree_tips)

# Testing for phylogenetic signal
result <- phylo.d(data = comp_d,
                  binvar = d_var, 
                  permut = 10000)  #add a 0 for real analyses

result
plot(result)

#### Main Text, Results: Are Supernatural Explanations More Prevalent for Social or Natural Phenomena? ####

# Calculate the distribution of the number of supernatural explanations
table(rowSums(data.frame(d$V_1s_common, d$V_2s_common, d$V_3s_common,
                         d$V_4s_common, d$V_5s_common, d$V_6s_common), na.rm=T))

# Calculate median number of supernatural explanations by society
median(rowSums(data.frame(d$V_1s_common,d$V_2s_common,d$V_3s_common,
                         d$V_4s_common,d$V_5s_common,d$V_6s_common),na.rm=T))

# How common are the explanations in ethnography?
mean(d$V_1s_common, na.rm = T) #Pathogens
mean(d$V_2s_common, na.rm = T) #Scarcity
mean(d$V_3s_common, na.rm = T) #Hazards
mean(d$V_4s_common, na.rm = T) #Warfare
mean(d$V_5s_common, na.rm = T) #Murder
mean(d$V_6s_common, na.rm = T) #Theft

# Phylogenetically adjusted t-tests 
# ... comparing proportion of natural vs. social phenomena with supernatural explanations
phyl.pairedttest(tree = tree,
                 as.matrix(d[tree$tip.label,
                             c("Natural2", "Social2")]))

# Phylogenetically adjusted t-tests 
#' Comparing proportion of societies with at least one natural vs. social 
#' phenomenon with supernatural explanations
phyl.pairedttest(tree = tree,
                 as.matrix(d[tree$tip.label, 
                             c("Natural_binary", "Social_binary")]))

# Create Figure 2
# Read out the figure in dimensions 15 x 15

# Create a duplicate tree so that we can change the tip labels for the figure
tree2 <- tree 
tree2$tip.label <- d$Culture

plot.phylo(tree2,
           type = "tidy",
           label.offset = .10,
           font = 1,
           cex = .5,
           align.tip.label = T)

# The next block of code defines the variables to plot
d_bin <- d[,c("Culture",
              "tree_tips", 
              "Natural_binary",
              "Social_binary",
              "V_1s_common",
              "V_2s_common",
              "V_3s_common",
              "V_4s_common",
              "V_5s_common",
              "V_6s_common")]

plot_var1 <- "V_1s_common"
plot_var2 <- "V_2s_common"
plot_var3 <- "V_3s_common"
plot_var4 <- "V_4s_common"
plot_var5 <- "V_5s_common"
plot_var6 <- "V_6s_common"

# These lines of code define the color scheme
disease_colours <- ifelse(d_bin[, plot_var1] == 1, "mediumseagreen",
                          ifelse(d_bin[, plot_var1] == 0, "white",NA))
hazard_colours  <- ifelse(d_bin[, plot_var2] == 1, "turquoise4",
                          ifelse(d_bin[, plot_var2] == 0, "white",NA))
famine_colours  <- ifelse(d_bin[, plot_var3] == 1, "dodgerblue4",
                          ifelse(d_bin[, plot_var3] == 0, "white",NA))
war_colours     <- ifelse(d_bin[, plot_var4] == 1, "firebrick4",
                          ifelse(d_bin[, plot_var4] == 0, "white",NA))
murder_colours  <- ifelse(d_bin[, plot_var5] == 1, "orange3",
                          ifelse(d_bin[, plot_var5] == 0, "white",NA))
theft_colours   <- ifelse(d_bin[, plot_var6] == 1, "tomato3",
                          ifelse(d_bin[, plot_var6] == 0, "white",NA))

disease_colours <- ifelse(is.na(disease_colours) == T, "gray", disease_colours)
hazard_colours <- ifelse(is.na(hazard_colours) == T, "gray", hazard_colours)
famine_colours <- ifelse(is.na(famine_colours) == T, "gray", famine_colours)
war_colours <- ifelse(is.na(war_colours) == T, "gray", war_colours)
murder_colours <- ifelse(is.na(murder_colours) == T, "gray", murder_colours)
theft_colours <- ifelse(is.na(theft_colours) == T, "gray", theft_colours)

# This now adds the color-coded tip labels
tiplabels(bg = disease_colours,
          cex = 1.5,
          adj = .51,
          pch = 21,
          aligntips = T)

tiplabels(bg = hazard_colours,
          cex = 1.5,
          adj = .525,
          pch = 21,
          aligntips = T)

tiplabels(bg = famine_colours,
          cex = 1.5,
          adj = .54,
          pch = 21,
          aligntips = T)

tiplabels(bg = war_colours,
          cex = 1.5,
          adj = .555,
          pch = 21,
          aligntips = T)

tiplabels(bg = murder_colours,
          cex = 1.5,
          adj = .57,
          pch = 21,
          aligntips = T)

tiplabels(bg = theft_colours,
          cex = 1.5,
          adj = .585,
          pch = 21,
          aligntips = T)

#' Association between frequency of phenomenon and frequency of supernatural 
#' explanation for four phenomena 
#' Phylogenetically controlled logistic regression

# Pathogen threat
fit1 <- phyloglm(V_1s_common~pathogensum, data = d, 
               phy = unroot(tree), 
               method = c("logistic_MPLE"))
summary(fit1)
exp(cbind(coef(fit1), confint(fit1)))

# Natural causes of scarcity
fit2 <- phyloglm(V_2s_common ~ famine_index, data = d, 
               phy = unroot(tree), 
               method = c("logistic_MPLE"))
summary(fit2)
exp(cbind(coef(fit2), confint(fit2)))

# Natural hazards
fit3<-phyloglm(V_3s_common ~ oth, data = d, 
               phy = unroot(tree), 
               method = c("logistic_MPLE"))
summary(fit3)
exp(cbind(coef(fit3), confint(fit3)))

# Warfare (internal and external)
fit4<-phyloglm(V_4s_common ~ internalwar+externalwar, data = d, 
               phy = unroot(tree), 
               method = c("logistic_MPLE"))
summary(fit4)
exp(cbind(coef(fit4), confint(fit4)))

#### Main Text, Results: How is Social Complexity Associated with the Prevalence of Supernatural Explanations? ####

#' Start here by creating some variables which allow us to... 
#' Probe estimates at different levels of social complexity and supernatural 
#' explanation prevalence 
d$Natural2_c <- d$Natural2 - mean(d$Natural2,na.rm=T)
d$Social2_c <- d$Social2 - mean(d$Social2,na.rm=T)

d$complexity_c <- d$complexity - mean(d$complexity, na.rm=T)
d$complexity_l <- d$complexity - (mean(d$complexity, na.rm=T) - sd(d$complexity, na.rm = T))
d$complexity_h <- d$complexity - (mean(d$complexity, na.rm=T) + sd(d$complexity, na.rm = T))

# Now we will fit the key social complexity models

# Unstandardized models
#' To estimate the prevalence of socially focused supernatural explanations at 
#' high and low complexity
#' ... change the "complexity_c" variable to "complexity_l" or "complexity_h"
fit1a_phylo <- phylolm(Social2 ~ complexity_c + Natural2_c,  #"A"
                              data = d, 
                              phy = unroot(tree))
summary(fit1a_phylo)
confint(fit1a_phylo)

fit2a_phylo <- phylolm(Natural2 ~ complexity_h + Social2_c,  #"A"
                       data = d, 
                       phy = unroot(tree))
summary(fit2a_phylo)
confint(fit2a_phylo)

# Standardized models
summary(fit1b_phylo <- phylolm(scale(Social2) ~ scale(complexity) + 
                                 scale(Natural2), #"B"
                               data = d, 
                               phy = unroot(tree)))

summary(fit2b_phylo <- phylolm(scale(Natural2) ~ scale(complexity) + 
                                 scale(Social2), #"B"
                              data = d, 
                              phy = unroot(tree)))

#' Exploratory models including specific social complexity indicators
#' This code loops through the indicators of the social complexity scale and 
#' refits general linear models
#' The effect size coefficient and confidence intervals of the models are entered into a dataset 
#' The resulting dataset is used to create Figure 3 (Figure 3 is created in ppt)
Fig3effects <- data.frame("Domain_of_Explanation"=NA,"IV"=NA,"B"=NA,"LLCI"=NA,"ULCI"=NA) 

for (i in 52:62) {  # 52-62 is the range of social complexity indicators
  
  fit1<-phylolm(scale(Social2) ~ scale(d[,i]) + scale(Natural2),
                data = d,phy = unroot(tree))
  Fig3effects[i-51,1] <- "Social"
  Fig3effects[i-51,2] <- colnames(d)[i]
  Fig3effects[i-51,3] <- fit1$coefficients[2]
  Fig3effects[i-51,4] <- confint(fit1)[2,1]
  Fig3effects[i-51,5] <- confint(fit1)[2,2]
  
  fit2<-phylolm(scale(Natural2) ~ scale(d[,i]) + scale(Social2),
                data = d,phy = unroot(tree))
  Fig3effects[i-40,1] <- "Natural"
  Fig3effects[i-40,2] <- colnames(d)[i]
  Fig3effects[i-40,3] <- fit2$coefficients[2]
  Fig3effects[i-40,4] <- confint(fit2)[2,1]
  Fig3effects[i-40,5] <- confint(fit2)[2,2]
}

# Another loop to estimate the values in the bottom panel of Figure 3
# Start by creating a dataset with the values and then populate them
# The resulting dataset is used to create Figure 3 (Figure 3 is created in ppt)
Fig3quartiles <- data.frame("phenomenon"=NA,"quart"=NA,"mean"=NA) 

for (i in 1:4) {
  
  quarts<-quantile(d$complexity)
  d$complexity_quart <- d$complexity - quarts[[i]]
  
  path<-phylolm(V_1s_common ~ complexity_quart, data = d, 
                 phy = unroot(tree))
  scar<-phylolm(V_2s_common ~ complexity_quart, data = d, 
                 phy = unroot(tree))
  haza<-phylolm(V_3s_common ~ complexity_quart, data = d, 
                 phy = unroot(tree))
  warf<-phylolm(V_4s_common ~ complexity_quart, data = d, 
                 phy = unroot(tree))
  murd<-phylolm(V_5s_common ~ complexity_quart, data = d, 
                 phy = unroot(tree))
  thef<-phylolm(V_6s_common ~ complexity_quart, data = d, 
                 phy = unroot(tree))
  
  if (i == 1) {r = 1} 
  if (i == 2) {r = 7} 
  if (i == 3) {r = 13} 
  if (i == 4) {r = 19} 
  
  Fig3quartiles[r,1] <- "path"
  Fig3quartiles[r+1,1] <- "scar"
  Fig3quartiles[r+2,1] <- "haza"
  Fig3quartiles[r+3,1] <- "warf"
  Fig3quartiles[r+4,1] <- "murd"
  Fig3quartiles[r+5,1] <- "thef"
  
  Fig3quartiles[r,2] <- i
  Fig3quartiles[r+1,2] <- i
  Fig3quartiles[r+2,2] <- i
  Fig3quartiles[r+3,2] <- i
  Fig3quartiles[r+4,2] <- i
  Fig3quartiles[r+5,2] <- i
  
  Fig3quartiles[r,3] <- path$coefficients[[1]]
  Fig3quartiles[r+1,3] <- scar$coefficients[[1]]
  Fig3quartiles[r+2,3] <- haza$coefficients[[1]]
  Fig3quartiles[r+3,3] <- warf$coefficients[[1]]
  Fig3quartiles[r+4,3] <- murd$coefficients[[1]]
  Fig3quartiles[r+5,3] <- thef$coefficients[[1]]
}

#### Supplemental Analyses ####
# Analyses Presented In The same Order As In The Supplemental Materials

#### Supplement: Distribution of Social Complexity, See Fig S1 ####

# This plots the histogram for the full set of 114 coded societies
setwd(dirname(getActiveDocumentContext()$path))

all <- read.csv("All Complexity.csv")
full <- read.csv("Processed_data Revision.csv") %>% as.data.frame()

ggplot(full,aes(x = complexity))+
  geom_histogram(color = "black", fill = "salmon", binwidth = .2)+
  theme_classic()

# This plots the histogram for the truncated set of 107 coded societies
setwd(dirname(getActiveDocumentContext()$path))

trunc <- read.csv("Processed_data Revision.csv") %>% as.data.frame()
trunc <- trunc[trunc$New == 0, ] 

ggplot(trunc,aes(x = complexity))+
  geom_histogram(color = "black", fill = "skyblue", binwidth = .2)+
  theme_classic()

# This plots the histogram for all societies in the SCCS for reference

ggplot(all,aes(x = complexity))+
  geom_histogram(color = "black", fill = "gray", binwidth = .2)+
  theme_classic()

#### Supplement: Full Models for Social Complexity Indicators Listed in Figure 3 (for Table S2) ####

# Creates empty list of regression models to populate
List_social <- list()
List_natural <- list()

# Loops through the social complexity indicators and populates list of models
for (i in 52:62) {
  
  varname <- colnames(d)[i]
  
  List_social[[varname]] <- phylolm(scale(Social2) ~ scale(d[,i]) + 
                                      scale(Natural2),
                                    data = d, phy = unroot(tree))
  List_natural[[varname]] <- phylolm(scale(Natural2) ~ scale(d[,i]) + 
                                       scale(Social2),
                                     data = d, phy = unroot(tree))
}

summary(List_social[["population"]]) #Retrieve the full model using the name of the complexity indicator 
summary(List_natural[["population"]]) #Retrieve the full model using the name of the complexity indicator 

#### Supplement: Replicating with Distinct Murder and Disease Codes ####

# Total number of explanations present? 
mean(d$V_1s_common_distinct, na.rm = T) 
mean(d$V_2s_common, na.rm = T) 
mean(d$V_3s_common, na.rm = T) 
mean(d$V_4s_common, na.rm = T) 
mean(d$V_5s_common_distinct, na.rm = T)
mean(d$V_6s_common, na.rm = T) 

# Phylogenetically adjusted t-tests comparing which attributions are most common 

phyl.pairedttest(tree = tree,
                 as.matrix(d[tree$tip.label,
                             c("Natural2_distinct", "Social2_distinct")]))

#Relationship with social complexity

fit1a_phylo <- phylolm(Social2_distinct ~ complexity_c + Natural2_distinct,  
                       data = d, 
                       phy = unroot(tree))
summary(fit1a_phylo)
confint(fit1a_phylo)

fit2a_phylo <- phylolm(Natural2_distinct ~ complexity_c + Social2_distinct,  
                       data = d, 
                       phy = unroot(tree))
summary(fit2a_phylo)
confint(fit2a_phylo)

#### Supplement: Present Rather than Common ####

# Creating new composite dataframes for natural and social events
Natural_events_present <- data.frame("disease" = d$V_1s_present,
                                     "scarcity" = d$V_2s_present,
                                     "hazards" = d$V_3s_present)
                                     
Social_events_present <- data.frame("war" = d$V_4s_present,
                                    "murder" = d$V_5s_present,
                                    "theft" = d$V_6s_present)

Total_events_present <- data.frame("disease" = d$V_1s_present,
                                   "scarcity" = d$V_2s_present,
                                   "hazards" = d$V_3s_present,
                                   "war" = d$V_4s_present,
                                   "murder" = d$V_5s_present,
                                   "theft"=d$V_6s_present)

#Creating new composite variables
d$Natural2_present <- rowSums(Natural_events_present, na.rm = T)
d$Social2_present <- rowSums(Social_events_present, na.rm = T)
d$Natural_binary_present <- ifelse(rowSums(Natural_events_present,na.rm = T) > 0, 1, 0)
d$Social_binary_present <- ifelse(rowSums(Social_events_present,na.rm = T) > 0, 1, 0)
d$total_events2_present <- rowSums(Total_events_present, na.rm = T)

# How common are the events in ethnography?
mean(d$V_1s_present, na.rm=T) #Pathogens 
mean(d$V_3s_present, na.rm=T) #Hazards   
mean(d$V_2s_present, na.rm=T) #Scarcity  
mean(d$V_4s_present, na.rm=T) #Warfare   
mean(d$V_5s_present, na.rm=T) #Murder    
mean(d$V_6s_present, na.rm=T) #Theft    

# Refit phylogenetically nested t-tests
phyl.pairedttest(tree = tree,
                 as.matrix(d[tree$tip.label,
                             c("Natural2_present", "Social2_present")]))

# Refit social complexity models
fit1a_phylo <- phylolm(Social2_present ~ complexity_c + Natural2_present,  
                       data = d, 
                       phy = unroot(tree))
summary(fit1a_phylo)
confint(fit1a_phylo)

fit2a_phylo <- phylolm(Natural2_present ~ complexity_c + Social2_present,  
                       data = d, 
                       phy = unroot(tree))
summary(fit2a_phylo)
confint(fit2a_phylo)

#### Supplement: Quasi-Poisson Models ####
summary(fit1 <- glm(Social2 ~ complexity + Natural2, 
                    family = quasipoisson, data = d)) 
summary(fit2 <- glm(Natural2 ~ complexity + Social2, 
                    family = quasipoisson, data = d)) 

#### Supplement: Case-weights by Confidence####
summary(lm(Social2 ~ complexity + Natural2, 
           data = d, weights = Society_conf)) 
summary(lm(Natural2 ~ complexity + Social2, 
           data = d, weights = Society_conf))

#### Supplement: Relationship with Social Cohesion and Inequality ####
fit3_phylo <- phylolm(Social2 ~ political.inequality + Natural2_c, 
                       data = d, 
                       phy = unroot(tree))
summary(fit3_phylo)
confint(fit3_phylo)

fit4_phylo <- phylolm(Natural2 ~ political.inequality + Social2_c, 
                       data = d, 
                       phy = unroot(tree))
summary(fit4_phylo)
confint(fit4_phylo)

fit5_phylo <- phylolm(Social2 ~ political.cohesion + Natural2_c, 
                       data = d, 
                       phy = unroot(tree))
summary(fit5_phylo)
confint(fit5_phylo)

fit6_phylo <- phylolm(Natural2 ~ political.cohesion + Social2_c,  
                       data = d, 
                       phy = unroot(tree))
summary(fit6_phylo)
confint(fit6_phylo)

#'###' Supplement: Supernatural Explanations in Societies Practicing 
#'Pastoral Subsistence
fit7_phylo <- phylolm(Social2 ~ pastoral + Natural2_c, 
                      data = d, 
                      phy = unroot(tree))
summary(fit7_phylo)
confint(fit7_phylo)

fit8_phylo <- phylolm(Natural2 ~ pastoral + Social2_c,  
                      data = d, 
                      phy = unroot(tree))
summary(fit8_phylo)
confint(fit8_phylo)

