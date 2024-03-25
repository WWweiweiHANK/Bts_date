library(simglm)
library(lme4)
library(paramtest)
library(snow)
library(lmerTest)

### STEP1: DECLARE THE FOLLOWING FUNCTION WITH THE CUSTOMIZED ENTRIES

mlm_test <- function(simNum, 
                     lv2, 
                     lv1,
                     pi_2,
                     coefs) {
  
  cat(lv1, lv2, pi_2, coefs, "\n")
  
  data_str <- "cross"
  
  fixed1 <- ~ 1 + child + gender + education + employment + income + age + GDP + GINI  ### must look exactly as above. The only diffrence is the random effects part. 
  #fixed_param1 <- c(1, rep(.5,2), rep(-.5,2), .3, -.3, .3, -.3)
  
  if (coefs == 1) {
    fixed_param1 <- c(1, rep(.15,2), rep(-.15,2), .07, -.07, .07, -.07)
  } else if (coefs == 2) {
    fixed_param1 <- c(1, rep(.3,2), rep(-.3,2), .15, -.15, .15, -.15)
  } else if(coefs == 3) {
    fixed_param1 <- c(1, rep(.5,2), rep(-.5,2), .3, -.3, .3, -.3)
  }
  
  random1 <- ~ 1 
  random_param1 <- list(random_var = pi_2*3, 
                        rand_gen = 'rnorm', 
                        ther_sim = TRUE)
  error_var <- 3
  with_err_gen <- 'rnorm'
  
  cov_param1 <- list(dist_fun = c('sample', 'rbinom', "sample", "sample", "rlnorm", "rnorm", "rnorm", "rlnorm"),
                     var_type = c("level1","level1","level1","level1","level1","level1","level1","level2"),
                     opts = list(list(x = 1:5, replace = T, prob = c(.1,.2,.3,.2,.1)),
                                 list(size=1, prob=0.6),
                                 list(x = 1:5, replace = T, prob = c(.1,.2,.3,.2,.1)),
                                 list(x = 1:5, replace = T, prob = c(.1,.2,.3,.2,.1)),
                                 list(mean = 0, sd = 1),
                                 list(mean = 0, sd = 1),
                                 list(mean = 0, sd = 1),
                                 list(mean = 0, sd = 1)
                     ))
  
  ## simulates 2nd data set (needed for the likelihood ratio test of random effects)     	
  dat22<-simglm::sim_reg(fixed = fixed1, 
                         fixed_param = fixed_param1, 
                         random = random1,
                         random_param = random_param1, 
                         cov_param = cov_param1,
                         error_var = error_var,
                         with_err_gen = with_err_gen,
                         k = NULL, 
                         n = lv2, 
                         p = lv1,
                         data_str = data_str)
  
  return <- tryCatch({
    
    ### all the (nested) models are fitted to make sure the likelihood-ratio tests are possible
    mod2 <-   lmerTest::lmer(sim_data ~ 1 + child + gender + education + employment + income + age +
                            GDP + GINI +(1|clustID), 
                          data=dat22)
    
    p2 <- coef(summary(mod2))[2,5]
    p3 <- coef(summary(mod2))[3,5]
    p4 <- coef(summary(mod2))[4,5]
    p5 <- coef(summary(mod2))[5,5]
    p6 <- coef(summary(mod2))[6,5]
    p7 <- coef(summary(mod2))[7,5]
    p8 <- coef(summary(mod2))[8,5]
    
    sig2 <- p2 <.05
    sig3 <- p3 <.05
    sig4 <- p4 <.05
    sig5 <- p5 <.05
    sig6 <- p6 <.05
    sig7 <- p7 <.05
    sig8 <- p8 <.05
    
    return(c(sig2, sig3, sig4, sig5, sig6, sig7, sig8))
  },
  error=function(e) {
    
    return(c(sig2=NA, sig3=NA, 
             sig4=NA, sig5=NA, sig6=NA, 
             sig7=NA, sig8=NA))
  })
  
  return(return)
}

######### creates list of sample sizes at Level 1 and Level2 #########      


power_mlm2 <- grid_search(mlm_test, 
                         params=list(lv1=seq(from=10, to=50, by = 10), 
                                     lv2=seq(from=30, to=90, by= 30),
                                     pi_2 = c(.5,1),
                                     coefs = c(1,2,3)),
                         n.iter=500, 
                         output='data.frame', 
                         parallel="snow",
                         ncpus=8)
save.image()

b <- results_total <- results(power_mlm) %>%
  pivot_longer(cols = X1:X7) %>%
  mutate(coefs.test = case_when(
    coefs.test == 1 ~ "Ultra-low condition",
    coefs.test == 2 ~ "Low condition",
    coefs.test == 3 ~ "Medium condition"
  )) %>%
  mutate(coefs.test = as.factor(coefs.test)) %>%
  mutate (Item.type = case_when(
    name == "X1" | name == "X3" | name == "X4" ~ "Lvl1-Cont",
    name == "X2" ~ "Lvl1-Binary",
    name == "X5" | name == "X6" ~ "Lvl1-Cat",
    name == "X7" | name == "X8" ~ "Lvl2-Conti"
  )) %>%
  group_by(lv1.test,
           lv2.test,
           coefs.test,
           pi_2.test,
           Item.type) %>%
  summarise(power = mean(value, na.rm =T),
            na = sum (is.na(value)))

b$coefs.test <- factor(b$coefs.test,
                       c("Ultra-low condition",
                         "Low condition",
                         "Medium condition"))
ggplot(b, aes(x = lv1.test,
              y = power)) +
  geom_point(aes(colour = Item.type), size = 3, alpha = .90) +
  geom_hline(yintercept = 0.95, linetype = 1, size =1, color = 'red') +
  geom_smooth(linetype = 1, size = 1) +
  scale_y_continuous("Power", breaks = seq(0, 1, .2)) +
  theme_bw(base_size = 20) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank() )+
  facet_grid(lv2.test ~ coefs.test) +
  xlab("Individual level units")

ggsave(filename = "lmpower.tiff",
       width = 10,
       height = 8,
       device='tiff', dpi=700)


