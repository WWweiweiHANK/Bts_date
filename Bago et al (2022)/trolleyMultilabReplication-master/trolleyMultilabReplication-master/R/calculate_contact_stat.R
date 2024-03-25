#' Calculates the statistics for the main effect of physical contact
#'
#' This is the description
#'
#' @param data data frame or tibble that contains Region and specified variables
#' @param vars the variables to be collapsed
#' @param label what data label should be attached to the output
#' @param rscale prior to be passed to the ttestBF function
#'
#' @return a tibble that contains the selected statistics
#'   in a format that can be printed
#' @export
calculate_contact_stat <- function(data = NULL, vars = NULL, label = NULL, rscale) {
  data %>%
    tibble::as_tibble() %>%
    dplyr::select(Region, {{vars}}) %>%
    dplyr::group_by(Region) %>%
    tidyr::nest() %>%
    dplyr::arrange(Region) %>%
    dplyr::mutate(data_long = purrr::map(data,
                                         . %>%
                                           tidyr::pivot_longer(
                                             cols = everything(),
                                             names_to = "condition",
                                             values_to = "rate",
                                             values_drop_na = TRUE) %>%
                                           as.data.frame()),
                  bttest = purrr::map(data_long,
                                      ~BayesFactor::ttestBF(formula = rate ~ condition, data = .x,
                                                            rscale = rscale, nullInterval = c(0, Inf))),
                  hdi = purrr::map(data_long,
                                   ~ bayestestR::hdi(
                                     BayesFactor::ttestBF(formula = rate ~ condition, data = .x, rscale = rscale),
                                     c = 0.89)),
                  fttest = purrr::map(data_long,
                                      ~t.test(rate ~ condition, data = .x) %>%
                                        broom::tidy())) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Exclusion = label,
      Cluster = Region,
      BF = purrr::map_chr(bttest,
                          . %>%
                            tibble::as_tibble() %>%
                            slice(2) %>%
                            dplyr::pull(bf) %>%
                            scales::scientific()),
      t = purrr::map_dbl(fttest,
                         ~dplyr::pull(.x, statistic) %>%
                           round(2)),
      df = purrr::map_dbl(fttest,
                          ~dplyr::pull(.x, parameter) %>%
                            round(2)),
      p = purrr::map_chr(fttest,
                         ~dplyr::pull(.x, p.value) %>%
                           round(3)),
      `Cohen's d` = purrr::map_dbl(data_long,
                                   ~abs(
                                     effsize::cohen.d(rate ~ condition, data = .x)$estimate %>%
                                       round(2))),
      `Raw effect` = purrr::map_dbl(data_long,
                                    . %>%
                                      dplyr::group_by(condition) %>%
                                      dplyr::summarise(avg_rate = mean(rate, na.rm = TRUE),
                                                       .groups = "drop") %>%
                                      dplyr::mutate(condition = glue::glue("avg_{stringr::str_extract(condition, '[0-9]')}")) %>%
                                      tidyr::pivot_wider(names_from = condition, values_from = avg_rate) %>%
                                      dplyr::mutate(raw_effect = round(avg_4 - avg_1, 2)) %>%
                                      dplyr::pull(raw_effect)),
      CI = purrr::map_chr(hdi,
                          . %>%
                            as_tibble() %>%
                            mutate(CI = glue::glue("[{round(CI_low, 2)}, {round(CI_high, 2)}]")) %>%
                            pull(CI))
    )
}


