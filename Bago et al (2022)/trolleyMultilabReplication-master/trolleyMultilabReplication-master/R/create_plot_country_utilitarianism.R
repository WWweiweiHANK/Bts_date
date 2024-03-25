#' Create country and average moral acceptability rating scatter plot
#'
#' This function creates a ....
#'
#' @param data the prepared datatable for plotting
#'
#' @return The function returns a ggplot2 list object.
#' @export
create_plot_country_utilitarianism <- function(data) {
  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = mean,
      y = Collectivism,
      label = country) +
    ggplot2::geom_point(
      ggplot2::aes(
        size = N,
        color = Region)) +
    ggplot2::guides(size = FALSE) +
    ggplot2::geom_text(hjust = -0.2, size = 3) +
    ggplot2::geom_smooth(
      method = "lm",
      mapping = ggplot2::aes(weight = N),
      color = "blue",
      show.legend = FALSE,
      se = F) +
    ggplot2::xlab("Average moral acceptibility ratings") +
    ggplot2::scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15), limits = c(0, 0.15)) +
    ggplot2::scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6) )+
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", size = 17),
      axis.title.y = element_text(face = "bold", size = 10),
      axis.text.x = element_text(face = "bold", size = 10, colour = "Black"),
      axis.text.y = element_text(face = "bold", size = 12, colour = "Black"),
      legend.title = element_blank(),
      legend.text = element_text(size = 9))
}
