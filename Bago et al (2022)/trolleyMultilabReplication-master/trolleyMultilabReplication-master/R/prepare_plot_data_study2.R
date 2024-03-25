#' Prepare datatable for plotting for study2
#'
#' Prepares datatable for creating a plot for study1. The plot
#' can be created with the \code{\link{create_plot_study2}} function.
#'
#' @param data the datatable to be plotted
#' @param study_type the type of the study either "trolley" or "speedboat"
#'
#' @return the function returns a modified datatable
#' ready to be passed to the plotting function.
#' @export
prepare_plot_data_study2 <- function(data, study_type) {
  response_cols <-
    switch(study_type,
           "trolley" = c("trolley_3_rate", "trolley_4_rate", "trolley_5_rate", "trolley_6_rate"),
           "speedboat" = c("speedboat_3_rate", "speedboat_4_rate", "speedboat_5_rate", "speedboat_6_rate"))

  data %>%
    dplyr::select(survey_name, response_cols) %>%
    dplyr::mutate(survey_name = factor(survey_name)) %>%
    tidyr::pivot_longer(
      -survey_name,
      names_to = "condition",
      values_to = "rate") %>%
    dplyr::mutate(personal_force = dplyr::if_else(stringr::str_detect(condition, "4|6"), "Personal Force", "No personal force"),
           intention = dplyr::if_else(stringr::str_detect(condition, "3|6"), "No Intention", "Intention"),
           survey_name = stringr::str_remove(survey_name, "PSA006_")) %>%
    tidyr::drop_na(rate)
}

