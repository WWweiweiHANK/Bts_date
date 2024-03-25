#' Evaluate results lm
#'
#' Run a Bayesian ANOVA and return the BF.
#'
#' @param df TODO
#' @param groups TODO
#' @param prior TODO
#' @param bf_thresholds TODO
#'
#' @return A tibble with the grouping variables, a calculated BF,
#' and the inference.
#' @export
get_lmbf_inference <- function(df,
                               groups = c("study", "class"),
                               prior = prior,
                               # Evidence for the alternative and null, respectively
                               bf_thresholds = c(10, 1/10)
){
  suppressMessages(
    df %>%
      dplyr::group_nest(!!!syms(groups)) %>%
      dplyr::transmute(!!!syms(groups),
                       bf = purrr::map_dbl(data,
                                           ~(BayesFactor::lmBF(formula = choice ~ personal_force * intention,
                                                               rscaleFixed = prior,
                                                               data = as.data.frame(.x),
                                                               progress = FALSE) /
                                               BayesFactor::lmBF(formula = choice ~ personal_force + intention,
                                                                 rscaleFixed = prior,
                                                                 data = as.data.frame(.x),
                                                                 progress = FALSE)) %>%
                                             BayesFactor::extractBF(onlybf = TRUE)),
                       inference = dplyr::case_when(bf > bf_thresholds[1] ~ "replicated",
                                                    bf < bf_thresholds[2] ~ "not replicated",
                                                    TRUE ~ "inconclusive")
      )
  )
}
