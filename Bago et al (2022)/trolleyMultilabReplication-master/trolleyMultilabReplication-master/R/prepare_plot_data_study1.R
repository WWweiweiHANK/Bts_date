#' Prepare datatable for plotting for study1
#'
#' Prepares datatable for creating a plot for study1. The plot
#' can be created with the \code{\link{create_plot_study1}} function.
#'
#' @param data the datatable to be plotted
#' @param study_type the type of the study either "trolley" or "speedboat"
#'
#' @return the function returns a modified datatable
#' ready to be passed to the plotting function.
#' @export
prepare_plot_data_study1 <- function(data, study_type) {
  response_cols <-
    switch(study_type,
           "trolley" = c("trolley_1_rate", "trolley_2_rate"),
           "speedboat" = c("speedboat_1_rate", "speedboat_2_rate"))

  data %>%
    dplyr::select(survey_name, response_cols) %>%
    dplyr::mutate(survey_name = factor(survey_name)) %>%
    tidyr::pivot_longer(
      - survey_name,
      names_to = "condition",
      values_to = "rate") %>%
    dplyr::mutate(condition = dplyr::if_else(stringr::str_detect(condition, "2"),"No personal force", "Personal Force" ),
           condition = factor(condition, levels = c("No personal force","Personal Force")),
           survey_name = stringr::str_remove(survey_name, "PSA006_")) %>%
    tidyr::drop_na(rate)
}
