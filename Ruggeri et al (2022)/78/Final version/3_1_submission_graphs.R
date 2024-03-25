
##########################################################################################################
## This document will reproduce the analysis of the "The globalizability of temporal discounting". 
## Author 1, Author2, ...
## Submitted for publication to Nature Human Behavior
## This document is preregistered at OSF. 
## The following functions are necessary to handle data wrangling as well as to compute the different scores.
##########################################################################################################


# This series of scripts will reproduce the analysis of the "The globalizability of temporal discounting". This document is preregistered at OSF. 
## This script will reproduce the figures presented in the main text.

########################
# 0. Uility functions
########################

source("999_1_auxiliary_functions.R")

load("1_2_data_files.RData")
load("2_2_main_results.RData")

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("patchwork")) install.packages("patchwork")
if (!require("sjPlot")) install.packages("sjPlot")
if (!require("ggsci")) install.packages("ggsci")
if (!require("colorspace")) install.packages("colorspace")
if (!require("sf")) install.packages("sf")
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("rgeos")) install.packages("rgeos")
if (!require("extrafont")) install.packages("extrafont")
if (!require("ggeffects")) install.packages("ggeffects")
if (!require("meta")) install.packages("meta")
if (!require("gamm4")) install.packages("gamm4")

########################
# 1. Figure 2
########################
## This code will reproduce the  maps presented in Fig.2.

### We prepare a specific dataset for computing the graph
dat_map2 <- dat_unique %>% 
  group_by(Residence) %>% 
  summarise (score = mean(score),
             presbias = mean(presbias),
             absolmag = mean(absolmag),
             gainloss = mean(gainloss),
             delayspeed = mean(delayspeed),
             subaddit = mean(subaddit)) %>% 
  mutate (score = ntile(score, 5),
          presbias = ntile(presbias, 5),
          absolmag = ntile(absolmag, 5),
          gainloss = ntile(gainloss, 5),
          delayspeed  = ntile(delayspeed, 5),
          subaddit = ntile(subaddit, 5)) %>% 
  ungroup()

### We create a dataset with references for regions
world2 <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  mutate(name = fct_recode(name, "United_States" = "United States",
                           "United_Kingdom" = "United Kingdom",
                           "Bosnia_and_Herzegovina" ="Bosnia and Herz.",
                           "North_Macedonia" = "Macedonia",
                           "New_Zealand" = "New Zealand",
                           "South_Africa" = "South Africa",
                           "South_Korea" = "Korea"
  )) %>% 
  filter (region_un != "Antarctica")  %>%
  rename("Residence" = name) %>% 
  left_join(., dat_map2 , by= "Residence") 

### We load a personalized color pattern
pal <- c("#2067AC", "#92c4de", "#fddbc7", "#f3a582", "#b21e2d")

world2 <- world2 %>% 
  mutate(score = as.factor(score),
         presbias = as.factor(presbias),
         absolmag = as.factor(absolmag),
         gainloss = as.factor(gainloss),
         delayspeed = as.factor(delayspeed),
         subaddit = as.factor(subaddit),
         
         score = fct_recode(score, "Lowest" = "1","Highest" = "5"),
         presbias = fct_recode(presbias, "Lowest" = "1","Highest" = "5"),
         absolmag = fct_recode(absolmag, "Lowest" = "1","Highest" = "5"),
         gainloss = fct_recode(gainloss, "Lowest" = "1","Highest" = "5"),
         delayspeed = fct_recode(delayspeed, "Lowest" = "1","Highest" = "5"),
         subaddit = fct_recode(subaddit, "Lowest" = "1","Highest" = "5"))

### We estimate a map for each case (scores and anomalies) using a pre-created function
score_map <- graph_map_disc(world2, "score", 5, 20, pal = pal, title = "Temporal discount scores", lwd = .05)
presbias_map <- graph_map_disc(world2, "presbias", 0, 1, pal = pal, title = "Present bias", lwd = .05)
absolmag_map <- graph_map_disc(world2, "absolmag", 0, 1, pal = pal, title = "Absolute magnitude", lwd = .05)
gainloss_map <- graph_map_disc(world2, "gainloss", 0, 1, pal = pal, title = "Gain-loss assymetry", lwd = .05)
delayspeed_map <- graph_map_disc(world2, "delayspeed", 0, 1,pal = pal, title = "Delay-speedup", lwd = .05)
subaddit_map <- graph_map_disc(world2, "subaddit", 0, 1, pal = pal, title = "Subadditivity", lwd = .05)

### We create the map part of the figure & save it
Fig2A <- (score_map + presbias_map) / (absolmag_map + gainloss_map) / (delayspeed_map + subaddit_map) +
  plot_annotation(tag_levels = "A")+
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        plot.tag = element_text(size = 9, 
                                face = 'bold'))

## This code will reproduce the conditional effects for GINI on temporal discount scores (gini_scores_plot) and on the sum of anomalies presented in Fig.1.
### We employ a custom function to obtain the conditional non-linear effect of gini on scores.
gini_scores_plot <- res_graph(results_scores_lmer$mg1$gam, "GINI.gmc", "Temporal discount scores", "GINI", ymin =-1, ymax = 1)

### We calculate the sum of total anomalies per individual (excluding subadditivity)
Table_S13 <- dat_filter %>% 
  rowwise() %>%
  mutate(sum_ano = sum(presbias, absolmag, delayspeed, gainloss)) 
Table_S13$sum.gmcs = c(scale(Table_S13$sum_ano))


### We estimate a hierarchical model predicting the total number of anomalies
job::job({
  results_sum_anom_lmer <- Table_S13 %>% 
    select(sum.gmcs, 
           Residence, 
           ResponseId, 
           age.cwc, 
           Gender, 
           EducationCompleted, 
           Employment, 
           indineq.cwc,
           GDP.gmc, 
           GINI.gmc,
           inflation.gmc,
           debt.cwc ,
           assets.cwc) %>% 
    fit_models_lmer_ns(.)
}, import = c(Table_S13, fit_models_lmer_ns))


### We employ a custom function to obtain the conditional non-linear effect of gini on the total number of anomalies
gini_anomalies_plot <- res_graph(results_sum_anom_lmer$mg1, "GINI.gmc", "Number of anomalies", "GINI", ymin =-1, ymax = 1)

### We create the join plot of the effect of gini on scores and total number of anomalies
Fig2B <-  gini_scores_plot / gini_anomalies_plot + 
  plot_annotation(tag_levels = "A")+
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        plot.tag = element_text(size = 9, 
                                face = 'bold'))

### We create the final figure joining the previous two objects

Fig2 <- (Fig2A | Fig2B) + plot_layout(widths = c(2.8, 1)) +
  plot_annotation(tag_levels = "A")+
  theme(plot.tag = element_text(size = 9, 
                                face = 'bold'))

ggsave(Fig2, height =  7.5, width = 9, filename = "Fig2.tiff", device='tiff',  dpi=400)


########################
# 2. Figure 3
########################

## This code will reproduce Fig. 3. representing the effect of GDP on the scores per each block of items (gains, losses and larger gains)

### We select a customize palet of colors
pal <- pal_aaas("default", alpha = .4)(length(levels(dat_unique$Cntry_income)))
pal <- adjust_transparency(pal, alpha = 0.8)

### We calculate the scores for each block of questions
gains <- get_scores(get_latest_answer(dat_item, 6))$score
payments <- 5-get_scores(get_latest_answer(dat_item, 11))$score
largegains <- get_scores(get_latest_answer(dat_item, 16))$score

q <- dat_item %>% 
  count(ResponseId) %>% 
  arrange()        

data_graph <- dat_item %>% 
  arrange(ResponseId) %>%
  mutate (gains = rep(gains, q$n),
          payments = rep(payments, q$n),
          largegains = rep(largegains, q$n)) 

### We use a customize function to plot GDP against the total score per block
g10 <- ineq_grap(data_graph, gains, log(GDP), 0, 5, 0, 20, "log(GDP)", "Preference for immediate gains",pal)
g11 <- ineq_grap(data_graph, payments, log(GDP), 0, 5, 0, 20, "log(GDP)", "Preference for later payments", pal)
g12 <- ineq_grap(data_graph, largegains, log(GDP), 0, 5, 0, 20, "log(GDP)", "Preference for immediate large gains", pal)

### We join the three graphs into a single figure & save it
Fig3 <- (g10 + g11 + g12) +
  plot_annotation(tag_levels = "A")+
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom',
        plot.tag = element_text(size = 25, face = 'bold'))

ggsave(Fig3, height =  12, width = 22, filename = "Fig3.tiff", device='tiff', dpi=400)

########################
# 3. Figure 4
########################

## This section will reproduce Fig.3. The first part of the figure (above) will represent the percentage of individuals presenting a potential anomaly, and those who aligned with the theory. 
## The second figure (below) will compute the descriptive information for the temporal discount scores and anomalies (Below). Necessary code from other scripts is repeated here for convenience.
## Both figures are not merged in R, but using photoshop. We provide the code to reproduce both parts separated.

## Above figure: anomalies

### We select a customize palet of colors
pal <- pal_aaas("default", alpha = .6)(9)

### We recompute the anomalies rates
subaddit_all <- data.frame (sub1 = as.numeric(get_latest_answer(dat_item, 6)$choice + get_second_choices(dat_item,"Q16")),
                            sub2 = get_second_choices(dat_item,"Q17"))
anomalies_data <- list()
### We apply the function fixer_anom. Given a first and a second set of responses, plus an ID identifier, it will classify the pattern of responses in anomaly/not anomaly and in the first case, whether it is consistent or not. In the second case (not anomaly), it will also inform whether the first decision was a sooner or a later choice.
anomalies_data[[1]] <- fixer_anom (data.frame(fc = get_latest_answer(dat_item, 6)$choice, sc =  get_second_choices(dat_item,"Q16"), ResponseId = unique(dat_item$ResponseId)), type = "presbias")
anomalies_data[[2]] <- fixer_anom (data.frame(fc = get_latest_answer(dat_item, 6)$choice,sc =  as.numeric(get_latest_answer(dat_item, 16)$choice), ResponseId = unique(dat_item$ResponseId)), type = "absolmag")
anomalies_data[[3]] <- fixer_anom (data.frame(fc = get_latest_answer(dat_item, 6)$choice, sc =  as.numeric(get_latest_answer(dat_item, 11)$choice), ResponseId = unique(dat_item$ResponseId)), type = "gainloss")
anomalies_data[[4]] <- fixer_anom (data.frame(fc =  get_second_choices(dat_item,"Q18"), sc =  get_second_choices(dat_item,"Q19"), ResponseId = unique(dat_item$ResponseId)), type = "delay")
anomalies_data[[5]] <- fixer_anom (data.frame(fc = subaddit_all$sub1, sc =  subaddit_all$sub2, ResponseId = unique(dat_item$ResponseId)), type = "subaddit")

### We manipulate our dataset to present the rate of anomalies.
grap_anoms <- anomalies_data %>% 
  
  # We are only interested in individual responses
  reduce(left_join, by = "ResponseId") %>% 
  
  # We expand the dataframe so each row has a response of each individual
  pivot_longer(cols = starts_with("anomaly", ignore.case = FALSE), 
               names_to = "types", 
               values_to = "anom_type") %>% 
  
  # We specify the names of the anomalies correctly for the graphic.
  mutate (types = case_when(
    types == "anomaly.x" ~ "Present Bias",
    types == "anomaly.y" ~ "Absolute Magnitude",
    types == "anomaly.x.x" ~ "Gain-Loss Assym.",
    types == "anomaly.y.y" ~ "Delay-Speedup",
    types == "anomaly" ~ "Subaddivity")) %>% 
  
  # We select the responses we are interested on and merge this information with our previous expanded dataset (so in the future we can include other variables in the graphic)
  select(ResponseId, types, anom_type) %>% 
  inner_join(., dat_item, by = "ResponseId") %>% 
  
  # We aggregate the information and compute percentages
  group_by(Residence, Cntry_income, types) %>% 
  count(anom_type) %>% 
  mutate(suma = sum(n), prop = n/suma) %>%
  
  ## We only select the possible anomaly responses
  filter (anom_type == "Possible anomaly") %>% 
  ungroup() %>% 
  
  # We plot the data 
  ggplot(aes(y = prop,
             x = types,
             size = n,
             colour = factor(Cntry_income))) +
  geom_jitter(alpha = 0.6,
              pch=19,
              width = .1) +
  
  stat_summary(fun = "mean", 
               colour = "black", 
               size = .4, 
               alpha = .9, 
               geom = "crossbar")+  
  
  theme_classic(base_size = 15) +
  scale_size(range = c(3, 7), name="Sample size") + 
  geom_hline(yintercept = 0.5, linetype = "dashed", size = 1.5)+
  theme(legend.position = "noe")+
  ylab("Proportion of inconsistent choices") +
  xlab(NULL) +
  scale_colour_manual(values=pal,
                      name = "Proportion of inconsistent choices") +
  ylim(0,1) +
  bbc_theme() +
  guides(color = guide_legend(override.aes = list(size = 7)))

### We follow the same steps for the congruent responses.
grap_cong_rel <- anomalies_data %>% 
  
  reduce(left_join, by = "ResponseId") %>% 
  pivot_longer(cols = starts_with("response", ignore.case = FALSE), 
               names_to = "types", 
               values_to = "anom_type") %>% 
  mutate (types = case_when(
    types == "response.x" ~ "Present Bias",
    types == "response.y" ~ "Absolute Magnitude",
    types == "response.x.x" ~ "Gain-Loss Assym.",
    types == "response.y.y" ~ "Delay-Speedup",
    types == "response" ~ "Subaddivity")) %>% 
  select(ResponseId, types, anom_type) %>% 
  inner_join(., dat_item, by = "ResponseId") %>%
  
  group_by(Residence, Cntry_income, types) %>% 
  count(anom_type) %>% 
  mutate(suma = sum(n), 
         prop = n/suma) %>%
  filter(str_detect(anom_type, 'ongruent')) %>%
  
  filter (anom_type == "Congruent") %>% 
  ungroup() %>% 
  ggplot(aes(y = prop,
             x = types,
             size = n,
             colour = factor(Cntry_income))) +
  geom_jitter(alpha = 0.6,
              pch=19,
              width = .1) +
  
  stat_summary(fun = "mean", 
               colour = "black", 
               size = .4, 
               alpha = .9, 
               geom = "crossbar")+  
  
  theme_classic(base_size = 15) +
  scale_size(range = c(3, 7), name="Sample size") + 
  geom_hline(yintercept = 0.5, linetype = "dashed", size = 1.5)+
  theme(legend.position = "below")+
  xlab(NULL) +
  ylab("Proportions of congruent anomalies") +
  scale_colour_manual(values=pal,
                      name = "Proportions of anomalies") +
  ylim(0,1.05) +
  bbc_theme() +
  guides(color = guide_legend(override.aes = list(size = 7)))

## We merge both plots into a single figure $ save it.
Fig4A <- grap_anoms /grap_cong_rel + 
  plot_annotation(tag_levels = "A") +
  plot_layout(nrow = 2, ncol = 1,
              guides = "collect") & 
  theme(legend.position = 'top',
        plot.tag = element_text(size = 20, 
                                face = 'bold'))

ggsave(Fig4A, height =  10, width = 20, filename = "Fig4A.tiff", device='tiff', dpi=400)
rm(list = c("grap_anoms", "grap_cong_rel", "anomalies_data"))


## Below figure: average scores

### We select a customize colour patern
pal <- pal_aaas("default", alpha = .4)(length(levels(dat_unique$Cntry_income)))

### We compute average and standard deviation of scores at country-level.
dat_test <- dat_filter %>% 
  group_by(Code) %>% 
  summarise(mean = mean(score),
            sd = sd(score),
            n = n())

### We compute a random-effects meta-analysis for the scores 
meta_scores <- metamean(mean = mean,
                        sd = sd,
                        n = n, 
                        studlab=Code, 
                        data = dat_test,
                        method.tau="REML",
                        prediction = T)

### We compute a random-effects meta-analysis for the anomalies
meta_anoms <- list()
vars <- c("presbias", "absolmag", "gainloss", "delayspeed", "subaddit")
for (i in 1:length(vars)) {
  meta_anoms[[i]] <- metanal_anom_prop(dat_filter, vars[i])
}

### We use a custom-function to estimate the by-country scores and anomalies rates
score_descr <- descript_score(data = meta_scores, ylim2 = 19, lab = "Temporal discount scores", pal=pal)
presbias_descr <- descript_anom(data = meta_anoms[[1]], ylim2 = 1, lab = "Present bias", pal=pal)
absolmag_descr <- descript_anom(data = meta_anoms[[2]], ylim2 = 1, lab = "Absolute magnitude", pal=pal)
gainloss_descr <- descript_anom(data = meta_anoms[[3]], ylim2 = 1, lab = "Gain-loss assymetry", pal=pal)
delayspeed_descr <- descript_anom(data = meta_anoms[[4]], ylim2 = 1, lab = "Delay-speedup", pal=pal)
subaddit_descr <- descript_anom(data = meta_anoms[[5]], ylim2 = 1, lab = "Subadditivity", pal=pal)

### We create the final figure & save the graph.
Fig4B <- (score_descr + presbias_descr + absolmag_descr) / (gainloss_descr + delayspeed_descr + subaddit_descr) +
  plot_annotation(tag_levels = "A")+
  plot_layout(guides = "collect") & 
  theme(legend.position = 'none',
        axis.title.x=element_blank(),
        axis.text.x= element_text(size = 9),
        plot.tag = element_text(size = 9, face = 'bold'))

ggsave(Fig4B, height =  10, width = 12, filename = "Fig4B.tiff", device='tiff',dpi=400)


########################
# 4. Figure 5
########################
## The following code will compute Fig.5, exploring the univariate effects of each predictor on temporal discount scores. 

pal <- pal_aaas("default", alpha = .4)(length(levels(dat_filter$Cntry_income)))
pal <- adjust_transparency(pal, alpha = 0.8)

### We compute the debt ratio to median income
data_graph <- dat_filter %>% 
  mutate(debt3 = debt/Country_income) %>% 
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  filter(debt3 < 100)

### We use a custom graph to create the bubble charts
des13 <- ineq_grap(data_graph, score.gmcs, GINI, -1.5, 1.5, 15, 80, "GINI", "Temporal discount score",pal)
des14 <- ineq_grap(data_graph, score.gmcs, log(GDP), -1.5, 1.5, 5, 20, "log(GDP)", "Temporal discount score", pal)
des16 <- ineq_grap(data_graph, score.gmcs, Inflation, -1.5, 1.5, -5, 100, "Inflation", "Temporal discount score", pal)
des15 <- ineq_grap(data_graph, score.gmcs, indineq, -1.5, 1.5, -50000, 150000, "Individual economic ineq.", " Temporal discount score", pal)
des18 <- ineq_grap(data_graph, score.gmcs, assets, -1.5, 1.5, 0, 500000, "Assets", "Temporal discount score", pal)
des19 <- ineq_grap(data_graph, score.gmcs, debt3, -1.5, 1.5, 0, 4, "Debt r/t median income", "Temporal discount score", pal)

### We create the join figure & save it.
Fig5 <- (des13 + des14 + des16)/(des15 +des18 + des19)  +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        plot.tag = element_text(size = 12, 
                                face = 'bold')) 
ggsave(Fig5,  height =  10, width = 12, filename = "Fig5.tiff", device='tiff', dpi=400)
