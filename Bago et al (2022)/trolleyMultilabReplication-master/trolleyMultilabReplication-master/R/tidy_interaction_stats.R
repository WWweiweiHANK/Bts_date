#' Tidy the results of the interaction analysis
#'
#' Lorem ipsum
#'
#' @param data the result of the calculate_interaction_stats function
#'
#' @return tidy table
#' @export
tidy_interaction_stats <- function(data) {
  data %>%
  dplyr::transmute(variable,
                   BF = purrr::map_dbl(datt3,
                                       ~ dplyr::slice(.x) %>%
                                         dplyr::pull(bf) %>%
                                         round(2)),
                   b = purrr::map_dbl(frequentist,
                                      ~ dplyr::filter(.x, term == "personal_force2:intention2:value") %>%
                                        dplyr::pull(estimate) %>%
                                        round(2)),
                   CI = purrr::map_chr(hdi,
                                       . %>%
                                         tibble::as_tibble() %>%
                                         dplyr::filter(Parameter == "personal_force:intention:value-1.&.1.&.value") %>%
                                         dplyr::mutate(CI = glue::glue("[{round(CI_low, 2)}, {round(CI_high, 2)}]")) %>%
                                         dplyr::pull(CI)),
                   lower = purrr::map_dbl(frequentist,
                                          ~ dplyr::filter(.x, term == "personal_force2:intention2:value") %>%
                                            dplyr::pull(conf.low) %>%
                                            round(4)),
                   higher = purrr::map_dbl(frequentist,
                                           ~ dplyr::filter(.x, term == "personal_force2:intention2:value") %>%
                                             dplyr::pull(conf.high) %>%
                                             round(4)),
                   p = purrr::map_dbl(frequentist,
                                      ~ dplyr::filter(.x, term == "personal_force2:intention2:value") %>%
                                        dplyr::pull(p.value) %>%
                                        round(3))
  )
}
