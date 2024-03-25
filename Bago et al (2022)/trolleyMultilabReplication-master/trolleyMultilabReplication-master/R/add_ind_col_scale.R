#' Add individualism and collectivism aggregated scores
#'
#' The function takes a dataframe with the raw individual and
#' collectivism scales and calculates the aggregated scores row wise.
#'
#' @param df a dataframe that contains the raw scales
#'
#' @return The function returns the dataframe with the
#' additional aggregated scores added in four new
#' variables row wise: `hor_ind`, `ver_ind`, `hor_col`,
#' `ver_col`.
#' @export
add_ind_col_scale <- function(df) {
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      hor_ind = mean(c(individualism_scale_1, individualism_scale_2, individualism_scale_3, individualism_scale_4)),
      ver_ind = mean(c(individualism_scale_5, individualism_scale_6, individualism_scale_7, individualism_scale_8)),
      hor_col = mean(c(individualism_scale_9, individualism_scale_10, individualism_scale_11, individualism_scale_12)),
      ver_col = mean(c(individualism_scale_13, individualism_scale_14, individualism_scale_15, individualism_scale_16))) %>%
  dplyr::ungroup()
}
