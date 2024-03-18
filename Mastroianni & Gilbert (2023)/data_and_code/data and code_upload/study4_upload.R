##Example R code for analyses in Study 4 of "The Illusion of Moral Decline" (Mastroianni & Gilbert, 2023)

#####IMPORTANT NOTES#####
##We cannot provide most of the data necessary to run this code because it's proprietary
##And the owners of the data could sue us for posting it
##Some of the data is publicly available, and you can access it using the links in study4_us.csv and study4_non_us.csv
##Below are representative examples of the analyses we did on each data set.

##Frequentist
moral_mod <- lm(response ~ year, data = dat)
summary(moral_mod)

##Bayesian
moral_bayes <- stan_glm(response ~ year, data = dat)
describe_posterior(moral_bayes)