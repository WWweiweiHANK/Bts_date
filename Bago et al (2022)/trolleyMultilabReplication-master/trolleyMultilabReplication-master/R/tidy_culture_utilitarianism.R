#' Tidy the results of the utilitarianism analysis
#'
#' Lorem ipsum
#'
#' @param data the result of the calculate_culture_utilitarianism function
#'
#' @return tidy table
#' @export
tidy_culture_utilitarianism <- function(data) {
  data %>%
    dplyr::transmute(variable,
                     BF = purrr::map_dbl(datt3, ~dplyr::slice(.x) %>%
                                           dplyr::pull(bf) %>%
                                           round(2)),
                     b = purrr::map_dbl(frequentist,
                                        ~dplyr::filter(.x, term == "value") %>%
                                          dplyr::pull(estimate) %>%
                                          round(2)),
                     lower = purrr::map_dbl(frequentist,
                                            ~dplyr::filter(.x, term == "value") %>%
                                              dplyr::pull(conf.low) %>%
                                              round(4)),
                     higher = purrr::map_dbl(frequentist,
                                             ~dplyr::filter(.x, term == "value") %>%
                                               dplyr::pull(conf.high) %>%
                                               round(4)),
                     p = purrr::map_dbl(frequentist,
                                        ~dplyr::filter(.x, term == "value") %>%
                                          dplyr::pull(p.value) %>%
                                          round(3))
    )
}
