#' Prepare data for the cultural distance and effect size comparison plot
#'
#' This function ...
#'
#' @param data datatable to be plotted
#' @param study_name name of the study, either study1a, study1b, study2a, or study2b
#'
#' @return The function returns a datatable ready to be plotted.
#' @export
prepare_plot_data_country_means <- function(data, study_type) {
  response_cols <-
    switch(study_type,
           "trolley" = c("trolley_1_rate", "trolley_2_rate", "trolley_3_rate", "trolley_4_rate", "trolley_5_rate", "trolley_6_rate"),
           "speedboat" = c("speedboat_1_rate", "speedboat_2_rate", "speedboat_3_rate", "speedboat_4_rate", "speedboat_5_rate", "speedboat_6_rate"))

  data %>%
    dplyr::select(country3, Region, tidyselect::all_of(response_cols)) %>%
    tidyr::pivot_longer(matches("_rate"), names_to = "condition", names_pattern = "(.*)_rate", values_to = "rate", values_drop_na = TRUE) %>%
    tidyr::nest(data = c(condition, rate)) %>%
    dplyr::left_join(., select(cultural_distance, Collectivism, country3), by = "country3") %>%
    dplyr::mutate(N = map_int(data, nrow)) %>%
    dplyr::mutate(mean =  dplyr::if_else(N >= 10,
                                         purrr::map_dbl(data,
                                                        ~ mean(.x$rate, na.rm=T)),
                                         NA_real_)) %>%
    dplyr::rename(country = country3) %>%
    dplyr::select(-data) %>%
    tidyr::drop_na(Collectivism, mean)
}
