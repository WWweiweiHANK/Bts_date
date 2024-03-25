# COVID-19 Leader Perceptions Pilot 2 (Power Analyses Aggregating Countries)
# REDACTED
# 11/02/20

### Set seed for simulations
rand_seed = 828
set.seed(rand_seed)

## Package Loading
library(tidyr)
library(dplyr)
library(Hmisc)
library(lme4)
library(lmerTest)
library(simr)
library(effectsize)

### Set Simulation Parameters
nsubs = 21000*.6
nsim = 1000
alpha = .005/2
show_progress = TRUE


## Data Loading
df <- read.csv("CLP_Pilot2_Data_Long.csv", header=TRUE,
               stringsAsFactors=FALSE)

# Get rid of vars we won't use
df <- select(df, -contains("OUS"), -contains("covid"), -moral, -redcross_reliable,    # Not used
             -gender, -race, # Recoded
             -trustOther, -trustworthy) # Composite

# Recode the non-numeric variables
df$gender_recode <- factor(df$gender_recode, levels=c("Female", "Male", "Other"))

df$race_recode <- factor(df$race_recode, levels=c("Other", "Asian", "Black", "White"))

df <- df %>% mutate_at(c('age', 'education', 'income', 'religiosity', 'pol_ideology', 
                         'support', 'avg_trust'), as.numeric)

# Capitalize scenarios
df <- df %>%
  mutate(dilemma = ifelse(dilemma == "ppe", "PPE", capitalize(dilemma)),
         dilemma = factor(dilemma, levels = c("Lockdown", "Ventilator", "PPE", "Medicine")))

df <- df %>%
  mutate(dimension = recode(dimension, "IH" = "Instrumental Harm", 
                            "IB" = "Impartial Beneficence") ,
         dimension = factor(dimension, levels = c("Instrumental Harm", "Impartial Beneficence")))

df <- df %>%
  mutate(argument = factor(argument, levels = c("Utilitarian", "Non-Utilitarian")))


### Check Factor Levels
# Dimension
print(unique(df$dimension))
# Argument
print(unique(df$argument))
# Dilemma
print(unique(df$dilemma))

## Effect coding
df <- mutate(df,
             argument_recode = recode(argument, "Non-Utilitarian" = -0.5, 
                                      "Utilitarian" = 0.5),
             dimension_recode = recode(dimension, "Instrumental Harm" = -0.5,
                                       "Impartial Beneficence" = 0.5))

## Set Contrasts
# options("contrasts") # View current contrasts
options(contrasts = c("contr.treatment", "contr.poly")) # Set contrasts
options("contrasts") # Confirm contrasts


## Prepare dvs & scale covariates

### Behavioral Task
df_beh <- df %>% 
  filter(exclude_behav == 0) %>% 
  filter(!is.na(behav)) %>% 
  mutate(answer_recode = recode(behav, "Utilitarian" = 1,
                                "Non-Utilitarian" = 0)) %>%
  ungroup() %>%
  select(-avg_trust) %>%
  na.omit() %>%
  # Scale
  mutate_at(.funs=list(~scale(., center = TRUE, scale=FALSE) %>% as.vector), 
            vars(age, education, income, religiosity, pol_ideology, support))

n=length(unique(df_beh$subId))
print(paste("Number of Subjects:", n))

beh_model <- glmer(answer_recode ~ gender_recode + age + race_recode + education + income
                   + pol_ideology + religiosity + support
                   + dimension_recode + (1|dilemma),
                   data=df_beh,  family=binomial(link = "logit"))

print(round(summary(beh_model)$coeff, 2))

## Extend to predefined subjects
beh_model_ext <- extend(beh_model, n=nsubs, along='subId')

## Power Analysis for a variety of effect sizes
summ <- data.frame()
for (or in seq(1.28, 1.33, .01)) {
  
  print(paste("Odds ratio:", or))
  cd <- log(or)*sqrt(3)/pi
  slope <- log(or)
  print(paste("Slope:", round(slope, 2)))
  fixef(beh_model_ext)['dimension_recode'] <- slope
  
  power <- powerSim(beh_model_ext, test=fixed('dimension_recode', 'z'),
                    nsim = nsim, alpha=alpha, progress=show_progress)
  
  print(power)
  summ <- rbind(summ, cbind(task='voting', nsubs=nsubs, 
                            or=round(or, 2), d=round(cd, 2), slope=round(slope, 2),
                            summary(power)))
  
  # For tracking progress
  write.csv(summ, "CLP_Pilot2_Power_VT_tmp.csv", row.names = FALSE)
  
}

write.csv(summ, "CLP_Pilot2_Power_VT.csv", row.names = FALSE)
