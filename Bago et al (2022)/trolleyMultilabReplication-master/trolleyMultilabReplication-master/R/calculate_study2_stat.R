#' Runs the statistical analysis for study2
#'
#' The function runs the statistical analysis for study2 a and b as
#' described in the manuscript.
#'
#' @param data data frame or tibble that contains Region and specified variables
#' @param vars the variables to be collapsed
#' @param label what data label should be attached to the output
#' @param rscaleFixed prior
#'
#' @return a tibble that contains the selected statistics
#'   in a format that can be printed
#' @export
calculate_study2_stat <- function(data = NULL,
                                  vars = NULL,
                                  label = NULL,
                                  rscaleFixed){

  data %>%
    tibble::as_tibble() %>%
    dplyr::select(Region, {{vars}}) %>%
    dplyr::group_by(Region) %>%
    tidyr::nest() %>%
    dplyr::arrange(Region) %>%
    dplyr::mutate(data_long = purrr::map(data,
                           ~tidyr::pivot_longer(.x,
                                         cols = everything(),
                                         names_to = "condition",
                                         values_to = "rate",
                                         values_drop_na = TRUE) %>%
                             dplyr::mutate(
                               personal_force = dplyr::if_else(stringr::str_detect(condition, "3|5"), 0, 1),
                               intention = dplyr::if_else(stringr::str_detect(condition, "4|5"), 1, 0),
                               personal_force2 = dplyr::if_else(stringr::str_detect(condition, "3|5"), -1, 1),
                               intention2 = dplyr::if_else(stringr::str_detect(condition, "4|5"), 1, -1),
                               personal_force = factor(personal_force),
                               intention = factor(intention))%>%
                             as.data.frame()),
           bmod_1 = purrr::map(data_long,
                        ~BayesFactor::lmBF(rate ~ personal_force * intention,
                              data = .x,
                              rscaleFixed = rscaleFixed)),
           bmod_2 = purrr::map(data_long,
                        ~BayesFactor::lmBF(rate ~ personal_force + intention,
                              data = .x)),
           bmod = purrr::map2(bmod_1, bmod_2,
                       ~BayesFactor::recompute(.x / .y, iterations = 50000) %>%
                         tibble::as_tibble()),
           hdi = purrr::map(data_long,
                            ~ bayestestR::hdi(
                              BayesFactor::lmBF(rate ~ personal_force * intention, data = .x, rscaleFixed = rscaleFixed),
                              c = 0.89)),
           fmod = purrr::map(data_long,
                      ~lm(rate ~ personal_force2 * intention2, data=.x) %>%
                        broom::tidy())) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Exclusion = label,
      Cluster = Region,
      BF = purrr::map_chr(bmod,
                   ~dplyr::slice(.x, 1) %>%
                     dplyr::pull(bf) %>%
                     scales::scientific()),
      RR = NA_character_,
      `b` = purrr::map_dbl(fmod,
                  ~dplyr::filter(.x, term == "personal_force2:intention2") %>%
                      dplyr::pull(estimate) %>%
                      round(3)),
#      df = purrr::map_chr(fmod,
#                   ~dplyr::filter(.x, term == "Residuals") %>%
#                     dplyr::pull(df) %>%
#                     paste0("1, ", .)),
      CI = purrr::map_chr(hdi,
                         . %>%
                           tibble::as_tibble() %>%
                           dplyr::filter(Parameter == "personal_force:intention-1.&.1") %>%
                           dplyr::mutate(CI = glue::glue("[{round(CI_low, 2)}, {round(CI_high, 2)}]")) %>%
                           dplyr::pull(CI)),
      p = purrr::map_chr(fmod,
                  ~dplyr::filter(.x, term == "personal_force2:intention2") %>%
                    dplyr::pull(p.value) %>%
                       round(3)),
      `Eta squared` = purrr::map_dbl(data_long,
                              ~aov(rate ~ personal_force*intention, data = .x) %>%
                                effectsize::eta_squared() %>%
                                tibble::as_tibble() %>%
                                dplyr::filter(Parameter == "personal_force:intention") %>%
                                dplyr::pull(Eta2_partial) %>%
                                round(3)),
      `Raw effect` = purrr::map_dbl(data_long,
                             ~dplyr::group_by(.x, personal_force, intention) %>%
                               dplyr::summarise(avg_rate = mean(rate, na.rm = TRUE),
                                         .groups = "drop") %>%
                               tidyr::pivot_wider(names_from = c(personal_force, intention),
                                           values_from = avg_rate) %>%
                               dplyr::mutate(raw_diff = (`0_0`-`1_0`) - (`0_1` - `1_1`)) %>%
                               dplyr::pull(raw_diff) %>%
                               round(2))

    )
}

