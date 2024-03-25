theme_trolley <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", size = 17),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.text.x = element_text(face = "bold", size = 10, colour = "Black"),
    axis.text.y = element_text(face = "bold", size = 10, colour = "Black"),
    legend.text = element_text(size = 9))
  }
