#####SUPPLEMENTARY CODE FOR "The Illusion of Moral Decline" (Mastroianni & Gilbert, 2023)
##this code produces the BEAM simulations in the Supplement

##nn = number of negative units of information
##np = number of positive units of information
##ap = average positivity of positive information (positive number)
##an = average negativity of negative information (negative number)
##rp = rate at which positivity of positive information decays
##rn = rate at which negativity of negative information decays
##t = time
require(ggplot2)
require(jtools)
require(plyr)

beam <- function(ap, np, rp, t, an, nn, rn){
  a <- max(c(np*(ap - rp*t),0)) + min(c(nn*(an + rn*t),0))
  print(a)}

####illusion of moral decline -- biased exposure, more bad than good####
affect <- vector()
time <- vector()
for(i in 0:20){
  a <- beam(ap = 5, np = 5, rp = .1, t = i, an = -5, nn = 15, rn = .2)
  affect[i] <- a
  time[i] <- i
}
affect
present <- rep(-47.5, times = length(affect))

for_graph <- cbind(affect, time, present)
for_graph <- as.data.frame(for_graph)
graph_melt <- reshape(for_graph, varying = c("affect","present"),
                      v.names = "morality", timevar = "type", times = c("Morality at T Perceived by Observer at T = 0","Morality at T Perceived by Observer at T"),
                      idvar = "participant", direction = "long")
graph_melt$time2 <- -graph_melt$time
graph_melt$plot <- rep("1", times = nrow(graph_melt))

require(jtools)
plot1 <- ggplot(graph_melt, aes(x = time2, y = morality, color = type)) +
  geom_point(size = 5) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.title = element_blank()) +
  theme_apa() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
        ) +
  scale_color_grey()


####illusion of moral improvement -- biased exposure, more good info than bad####
affect <- vector()
time <- vector()
#ap = 5
#np = 21
#rp = .1
#t = i
#an = -5
#nn = 10
#rn = .2

ap = 5
np = 15
rp = .1
t = i
an = -5
nn = 5
rn = .2
for(i in 0:20){
  a <- beam(ap = ap,np = np,rp = rp, t = i, an = an, nn = nn, rn = rn)
  affect[i] <- a
  time[i] <- i
}
affect
present <- rep((ap*np + an*nn), times = length(affect))

for_graph <- cbind(affect, time, present)
for_graph <- as.data.frame(for_graph)
graph_melt2 <- reshape(for_graph, varying = c("affect","present"),
                      v.names = "morality", timevar = "type", times = c("Morality at T Perceived by Observer at T = 0","Morality at T Perceived by Observer at T"),
                      idvar = "participant", direction = "long")
graph_melt2$time2 <- -graph_melt$time
graph_melt2$plot <- rep("2", times = nrow(graph_melt2))

plot2 <- ggplot(graph_melt2, aes(x = time2, y = morality, color = type)) +
  geom_point(size = 5) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.title = element_blank()) +
  theme_apa() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
  ) +
  scale_color_grey()

####no biased memory####
affect <- vector()
time <- vector()
for(i in 0:20){
  a <- beam(ap = 0, np = 0, rp = .1, t = i, an = 0, nn = 0, rn = .1)
  affect[i] <- a
  time[i] <- i
}

present <- rep(0, times = length(affect))

for_graph <- cbind(affect, time, present)
for_graph <- as.data.frame(for_graph)
graph_melt3 <- reshape(for_graph, varying = c("affect","present"),
                      v.names = "morality", timevar = "type", times = c("Morality at T Perceived by Observer at T = 0","Morality at T Perceived by Observer at T"),
                      idvar = "participant", direction = "long")
graph_melt3$time2 <- graph_melt3$time

graph_melt3$plot <- rep("3", times = nrow(graph_melt3))

plot3 <- ggplot(graph_melt3, aes(x = time2, y = morality, color = type)) +
  geom_point(size = 5, position = position_dodge(width = .5)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.title = element_blank()) +
  theme_apa() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
  ) +
  scale_color_grey()

####export for figure ED2####
require(dplyr)
plot_melt <- bind_rows(graph_melt, graph_melt2,graph_melt3)
graph_melt3$plot
ed2 <- ggplot(plot_melt, aes(x = time2, y = morality, color = type)) +
  geom_point(size = 5, position = position_dodge(width = .5)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.title = element_blank()) +
  theme_apa() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_blank()
        ) +
  scale_color_grey() +
  facet_grid(~plot, scales = "free")
