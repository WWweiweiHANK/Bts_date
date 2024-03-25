#' Create plot for study2
#'
#' The function creates a plot for study1 based on
#' prepared dataset created by \code{\link{prepare_plot_data_study2}}.
#'
#' @param data the datatable to be plotted
#'
#' @return The function returns a ggplot2 list object.
#' @export
create_plot_study2 <- function(data) {
  data %>%
    dplyr::group_by(survey_name, personal_force, intention) %>%
    dplyr::mutate(ggplot2::mean_cl_boot(rate)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot() +
    ggplot2::aes(
                  x = intention,
                  y = rate,
                  fill = personal_force,
                  color = personal_force) +
    geom_flat_violin(aes(y = rate,
                         color = personal_force),
                     position = position_nudge(x = .2, y = 0),
                     alpha = .8) +
    ggplot2::geom_point(aes(y = rate,
                            color = personal_force),
                        position = ggplot2::position_jitter(width = .15, height = .20),
                        size = .25,
                        alpha = 0.8,
                        show.legend = FALSE) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::scale_color_viridis_d() +
    ggnewscale::new_scale_color() +
    ggplot2::geom_pointrange(aes(color = personal_force,
                                 y = y, ymin = ymin, ymax = ymax),
                             size = .25,
                             position = position_nudge(x = .3, y = 0)) +
    ggplot2::scale_color_manual(values = c("white", "black")) +
    ggplot2::facet_wrap(~survey_name) +
    ggplot2::labs(y = "To what extent is this action morally acceptable?",
                  x = NULL) +
    ggplot2::coord_cartesian(ylim = c(.5, 9.5), clip = "off") +
    ggplot2::scale_y_continuous(breaks = 1:9, expand = c(0, 0)) +
    theme_trolley() +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = element_blank()) # This is needed bc of complicated new scales

}


