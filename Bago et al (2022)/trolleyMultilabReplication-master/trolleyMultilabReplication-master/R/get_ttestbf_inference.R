#' Evaluate results ttest
#'
#' Run a Bayesian t-test.
#'
#' @param df TODO
#' @param groups TODO
#' @param formula TODO
#' @param prior TODO
#' @param bf_thresholds TODO
#'
#' @return A tibble with the grouping variables, a calculated BF,
#' and the inference.
#' @export
#' @examples
#' \dontrun{
#' get_ttestbf_inference(full_data2, groups = c("study", "class"), formula = choice ~ condition, prior = "medium")
#' }
get_ttestbf_inference <- function(df,
                                  groups = c("study", "class"),
                                  formula = NULL,
                                  prior = prior,
                                  # Evidence for the alternative and null, respectively
                                  bf_thresholds = c(10, 1/10)
){
  suppressMessages(
    df %>%
      dplyr::group_nest(!!!syms(groups)) %>%
      dplyr::transmute(!!!syms(groups),
                       bf = purrr::map(data,
                                       ~BayesFactor::ttestBF(formula = formula,
                                                             nullInterval = c(0, Inf),
                                                             rscale = prior,
                                                             data = as.data.frame(.x)) %>%
                                         BayesFactor::extractBF(onlybf = TRUE)) %>%
                         # Get the second BF of the two
                         purrr::map_dbl(2),
                       inference = dplyr::case_when(bf > bf_thresholds[1] ~ "replicated",
                                                    bf < bf_thresholds[2] ~ "not replicated",
                                                    TRUE ~ "inconclusive"))
  )
}
