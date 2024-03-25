#' Prepare data for the cultural distance and effect size comparison
#'
#' This function ...
#'
#' @param data datatable to be plotted
#' @param study_name name of the study, either study1a, study1b, study2a, or study2b
#'
#' @return The function returns a datatable ready to be plotted.
#' @export
prepare_plot_data_country <- function(data, study_name) {
  response_cols <-
    switch(study_name,
           "study1a" = c("trolley_1_rate", "trolley_2_rate"),
           "study1b" = c("speedboat_1_rate", "speedboat_2_rate"),
           "study2a" = c("trolley_3_rate", "trolley_4_rate", "trolley_5_rate", "trolley_6_rate"),
           "study2b" = c("speedboat_3_rate", "speedboat_4_rate", "speedboat_5_rate", "speedboat_6_rate"))

  data%>%
    dplyr::select(country3, Region, tidyselect::all_of(response_cols)) %>%
    tidyr::pivot_longer(matches("_rate"), names_to = "condition", names_pattern = "(.*)_rate", values_to = "rate", values_drop_na = TRUE) %>%
    dplyr::mutate(personal_force = dplyr::if_else(stringr::str_detect(condition, "4|6"), 1, 0),
           intention = dplyr::if_else(stringr::str_detect(condition, "4|5"), 1, 0)) %>%
    tidyr::nest(data = c(condition, rate, personal_force, intention)) %>%
    dplyr::left_join(., select(cultural_distance, Collectivism, country3), by = "country3") %>%
    dplyr::mutate(N = map_int(data, nrow)) %>%
    dplyr::mutate(cohend =  dplyr::if_else(N >= 10,
                                           purrr::map_dbl(data,
                                                          purrr::possibly(
                                                            ~ effectsize::eta_squared(aov(rate ~ intention : personal_force, data = .x))$Eta2,
                                                            NA_real_)),
                                           NA_real_)) %>%
    dplyr::rename(country = country3) %>%
    dplyr::select(-data) %>%
    tidyr::drop_na(Collectivism, cohend)
}
