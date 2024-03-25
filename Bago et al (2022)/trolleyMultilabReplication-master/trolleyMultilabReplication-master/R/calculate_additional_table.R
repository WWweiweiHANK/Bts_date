#' Calculate the table stats for the additional analysis
#'
#' This is a super lazy function that needs to be generalized.
#'
#' @param data the piped reformatted data for the additional analysis
#' @export
calculate_additional_table <- function(data) {
  data %>%
    tidyr::drop_na(rate, comparison) %>%
    tidyr::nest(data = c(survey_id, task, rate)) %>%
    dplyr::mutate(bf_res = purrr::map(data, ~ BayesFactor::ttestBF(formula = rate ~ task, data = .x, rscale = 0.26)),
           bf = purrr::map_dbl(bf_res, ~ round(my_bf_extractor(.x)$bf, 2)),
           freq_res = purrr::map(data, ~ t.test(rate ~ task, data = .x,  paired = FALSE) %>% broom::tidy()),
           t = purrr::map_dbl(freq_res, ~ round(.x$statistic, 2)),
           p = purrr::map_dbl(freq_res, ~ round(.x$p.value, 2)),
           df = purrr::map_dbl(freq_res, ~ round(.x$parameter, 2)))
}
