
graph_anomalies_indiv <- function (dat_grap){
  des4 <- merge(dat_item, dat_grap, by= "ResponseId") %>% 
    group_by(Residence) %>% 
    count(anomaly) %>% 
    mutate(suma = sum(n), prop = n/suma) %>% 
    ggplot(aes(y = prop,
               x = anomaly,
               group = factor(anomaly),
               colour = factor(anomaly))) +
    geom_jitter(alpha = 0.9,
                size = 7,
                pch=19) +
    stat_summary(fun.data = mean_cl_normal, 
                 geom = "pointrange",
                 size = 1, color = "black") +
    theme_bw(base_size = 10) +
    theme(legend.position = "none")+
    ylab("Proportion of decisions") +
    xlab("Type of decision") +
    scale_colour_manual(values=palette_Dark2(length(table(dat_grap$anomaly))),
                        name = "Type of anomaly") +
    ylim(0,1)
  
  
  des5 <- merge(dat_item, dat_grap, by= "ResponseId") %>%
    filter(str_detect(response, 'ongruent')) %>%
    group_by(Residence) %>% 
    count(response) %>% 
    mutate(suma = sum(n), prop = n/suma) %>% 
    ggplot(aes(y = prop,
               x = response,
               group = factor(response),
               colour = factor(response))) +
    geom_jitter(alpha = 0.9,
                size = 7,
                pch = 19) +
    ylim(0,1)+
    stat_summary(fun.data = mean_cl_normal, 
                 geom = "pointrange",
                 size = 1, 
                 color = "black") +
    theme_bw(base_size = 10) +
    theme(legend.position = "none")+
    ylab("Proportion of total anomalies") +
    xlab("Type of anomaly") +
    #ylim(0,800)+
    scale_colour_manual(values=palette_Dark2(length(table(dat_grap$anomaly))),
                        name = "Type of anomaly")
  
  layout <- c(
    area(t = 1, l = 1, b = 6, r = 3),
    area(t = 1, l = 5, b = 8, r = 8))
  
  des6 <- des4 + des5 + 
    plot_layout(design = layout)
  
  return (des6)
  
}



ineq_grap <- function (data, var1, var2, rangex, rangey) {
  
  p1 <-  data %>% 
    arrange({{var2}}) %>% 
    group_by(Residence) %>% 
    summarise(var1 = mean({{var1}}), 
              n = n(),
              var2 = mean({{var2}})) %>% 
    ungroup() %>% 
    ggplot(aes(x=var1, 
               y= var2, 
               fill=Residence,
               label = levels(Residence))) +
    geom_point(alpha=0.9, size = 6, shape=21, color="black") +
    geom_text(hjust = 0, nudge_x = .02) +
    scale_colour_manual(values=palette_Dark2(length(table(dat_item$Residence))),
                        name = "Type of anomaly")+
    theme_bw(base_size = 10) +
    ggtitle(paste("Country and temporal discount scores")) +
    # geom_abline(intercept = 0, slope = .06, alpha = .90, linetype = "dashed") +
    ylab("Feature") +
    xlab("Dependent var") +
    xlim(0, rangex) +
    ylim(0, rangey) +
    theme(legend.position = "none") 
  return(p1)
}