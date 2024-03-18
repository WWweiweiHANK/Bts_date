

a <- read.csv("2a_melt.csv")
a$study <- rep("2a", times =nrow(a))
a_small <- data.frame(a$rating,a$year,a$study)
head(a_small)
names(a_small) <- c("rating","time","study")
a_small$time <- as.factor(as.character(a_small$time))
a_small$time <- factor(a_small$time, levels = c("2000","2010","2020"))

b <- read.csv("2b_melt.csv")
b$study <- rep("2b", times =nrow(b))
b_small <- data.frame(b$rating,b$year,b$study)
head(b_small)
names(b_small) <- c("rating","time","study")
b_small$time <- as.factor(as.character(b_small$time))
b_small$time <- factor(b_small$time, levels = c("2010","2012","2014","2016","2018","2020"))

c <- read.csv("2c_melt.csv")
c$study <- rep("2c", times = nrow(c))
c_small <- data.frame(c$rating, c$time, c$study)
names(c_small) <- c("rating","time","study")
c_small$time <- mapvalues(c_small$time, from = c("born","twenty","today"),
                          to = c("Year Born","Year Turned 20", "Current Year"))
c_small$time <- factor(c_small$time, levels = c("Year Born","Year Turned 20", "Current Year"))

all <- bind_rows(a_small,b_small,c_small)
all$time <- factor(all$time, levels = c("2000","2010","2012","2014","2016","2018","2020","Year Born","Year Turned 20", "Current Year"))

##plot
all_plot <- ggplot(all, aes(x = time, y = rating)) +
  geom_violin() +
  geom_point(position = position_jitter(height = .2, width = .2), color = "gray", alpha = .2) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, geom = "errorbar") +
  stat_summary(fun = "mean", size = 1) +
  ylab("Perceived Morality of People in Each Year") +
  xlab(label = NULL) +
  theme_apa() +
  facet_grid(.~study, scales = "free")
