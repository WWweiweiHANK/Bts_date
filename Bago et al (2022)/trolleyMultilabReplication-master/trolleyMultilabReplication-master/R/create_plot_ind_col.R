#' Create a plot to visualize the effect of individualism/collectivism scale on moral dilemmas
#'
#' This function ...
#'
#' @param data datatable to be plotted
#'
#' @return The function returns a ggplot2 list object.
#' @export
create_plot_ind_col <- function(data) {
  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      y = variable,
      x = b,
      width = 0.7) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "dodge",
      colour = "Black",
      width = 1.2) +
    ggplot2::geom_errorbar(
      aes(
        xmin = lower,
        xmax = higher),
      position = position_dodge(width = 0.7),
      colour = "Black",
      width = 0.2) +
    ggplot2::labs(x = "Std. Beta (SEM)",
                  y = NULL) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(breaks = seq(-.10, .10, .05),
                                limits = c(-0.10,0.10)) +
    theme_trolley()
}
